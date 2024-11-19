{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad (unless)
import Data.Either
import Data.Foldable
import Data.List.NonEmpty qualified as NE
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Distribution.Compiler (CompilerId (..))
import Distribution.Fields (PrettyField)
import Distribution.Fields.Pretty (CommentPosition (..), showFields)
import Distribution.Parsec
import Distribution.Simple.Utils (fromUTF8LBS)
import Distribution.System
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Utils.Json
import Distribution.Verbosity qualified as Verbosity

import Compat
import CondTree
    ( Cond
    , Env (..)
    , defragC
    , flattenCondTree
    , foldCondTree

    , pushConditionals
    , simplifyGenericPackageDescription, ppCondTree2, condTreeJson
    )

import FieldMap (FieldMap, toList, ppFieldMap)
import GenericPackageDescription (CondTree', Tree (..), foldTree, runGenericPackageDescription)
import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..))
import Text.PrettyPrint (Doc, text)
import Pretty (prettyField, ppCondition,  prettySection  )

data Opts = Opts
    { optsOutput :: Maybe FilePath
    , optsFlags :: FlagAssignment
    , optsArch :: Maybe Arch
    , optsOs :: Maybe OS
    , optsCompiler :: Maybe CompilerId
    }
    deriving Show

defaultOptions :: Opts
defaultOptions =
    Opts
        { optsOutput = mempty
        , optsFlags = mempty
        , optsArch = Nothing
        , optsOs = Nothing
        , optsCompiler = Nothing
        }

mkArgDescr :: Parsec t => String -> (t -> b) -> ArgDescr (Either String b)
mkArgDescr placeholder set = ReqArg (either Left (Right . set) . eitherParsec) placeholder

options :: [OptDescr (Either String (Opts -> Opts))]
options =
    [ Option
        ""
        ["out"]
        (ReqArg (\fn -> Right (\opts -> opts{optsOutput = Just fn})) "FILE")
        "Path for output"
    , Option
        "f"
        ["flag"]
        ( ReqArg
            (either Left (\flags -> Right (\opts -> opts{optsFlags = optsFlags opts <> flags})) . eitherParsec)
            "FLAG"
        )
        "Flags"
    , Option
        "c"
        ["compiler"]
        (mkArgDescr "COMPILER" (\comp opts -> opts{optsCompiler = Just comp}))
        "Compiler"
    , Option
        "p"
        ["platform"]
        (mkArgDescr "PLATFORM" (\(Platform arch os) opts -> opts{optsArch = Just arch, optsOs = Just os}))
        "Platform"
    , Option
        "o"
        ["os"]
        (mkArgDescr "OS" (\os opts -> opts{optsOs = Just os}))
        "Operating System"
    , Option
        "a"
        ["arch"]
        (mkArgDescr "ARCH" (\arch opts -> opts{optsArch = Just arch}))
        "Architecture"
    ]

header :: String
header = "Usage: cabal2sync [OPTION...] files..."

main :: IO ()
main = do
    (o, as, errs) <- getOpt RequireOrder options <$> getArgs
    unless (null errs) $ do
        putStrLn $ concat errs ++ usageInfo header options
        exitFailure

    let (errs', o') = partitionEithers o
        opts = foldl' (flip id) defaultOptions o'

    unless (null errs') $ do
        for_ errs' $ \e ->
            putStrLn $ "Error: " ++ e
        exitFailure

    case (opts, as) of
        (_, []) -> do
            putStrLn "No files provided"
            exitFailure
        (_, [fn_in]) ->
            doOne opts fn_in
        (Opts{optsOutput = Nothing}, paths) ->
            for_ paths $ doOne opts
        (Opts{optsOutput = Just _}, _ : _ : _) -> do
            putStrLn "Output to file is incompatible with multiple inputs"
            exitFailure

doOne :: Opts -> FilePath -> IO ()
doOne Opts{..} fn = do
    let env = Env optsOs optsArch optsCompiler optsFlags
    gpd <- readGenericPackageDescription Verbosity.silent Nothing (makeSymbolicPath fn)
    let simplifiedGpd = simplifyGenericPackageDescription env gpd

    let v = specVersion (packageDescription gpd)
        top :: FieldMap (Fragment Json)
        trees :: [(String, Tree (CondTree' (FieldMap (Fragment Json))))]

        (top, middle, trees) = runGenericPackageDescription v simplifiedGpd

    putStrLn "original"
    putStrLn $ pp1 trees

    let trees0 :: [(String, (Tree (CondTree' (FieldMap (NE.NonEmpty (Fragment Json))))))]
        trees0 = (fmap . fmap . fmap . fmap . fmap) NE.singleton trees

    putStrLn "wrap in NonEmpty"
    putStrLn $
        pp
            ( foldCondTree
                (\it ifs -> [prettyField n (something a) | (n, a) <- FieldMap.toList it] ++ concat ifs)
                ( \c thenTree ->
                    [ prettySection "if" [ppCondition c] thenTree
                    ]
                )
                ( \c thenTree elseTree ->
                    [ prettySection "if" [ppCondition c] thenTree
                    , prettySection "else" [] elseTree
                    ]
                )
            )
            trees0

    let trees1 :: [(String, Tree (FieldMap (CondTree' (Fragment Json))))]
        trees1 = (fmap . fmap . fmap) pushConditionals trees

    putStrLn "after pushConditionals"
    putStrLn $
        pp
            (\fm -> [prettyField n (something $ condTreeJson a) | (n, a) <- FieldMap.toList fm])
            trees1

    let trees2 :: [(String, Tree (FieldMap (Cond ConfVar (Fragment Json))))]
        trees2 = (fmap . fmap . fmap) (fmap flattenCondTree) trees1

    putStrLn "after flattenCondTree"
    putStrLn $
        pp (\fm -> [prettyField n (something a) | (n, a) <- FieldMap.toList fm]) trees2

    let trees3 :: [(String, Tree (FieldMap (Fragment Json)))]
        trees3 = (fmap . fmap . fmap) (fmap defragC) trees2

    putStrLn "trees':"
    putStrLn $ pp2 trees3

-- let json =
--         JsonObject $
--             mconcat
--                 [ [(name, toJSON value) | (name, value) <- FieldMap.toList top]
--                 , [(name, toJSON value) | (name, value) <- middle ++ trees']
--                 ]

-- maybe BL.putStr BL.writeFile optsOutput $ renderJson json

pp :: (a -> [PrettyField ()]) -> [(String, Tree a)] -> String
pp f =
    showFields (const NoComment)
        . foldMap
            ( \(name, tree) ->
                [ prettySection name [] $
                    foldTree f (map (\(n, t) -> prettySection n [] t)) tree
                ]
            )

-- \$ foldTree f (\n t -> [prettySection n [] t]) tree ]

pp1 :: [(String, Tree (CondTree' (FieldMap (Fragment Json))))] -> String
pp1 = pp (ppCondTree2 . fmap (fmap something))

pp2 :: ToJSON a => [(String, Tree (FieldMap a))] -> String
pp2 = pp (ppFieldMap . fmap something)

something :: ToJSON a => a -> Doc
something = text . fromUTF8LBS . renderJson . toJSON

