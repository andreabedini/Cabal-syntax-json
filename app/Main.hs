{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (unless)
import Data.ByteString.Lazy qualified as BL
import Data.Either
import Data.Foldable
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Distribution.Compiler (CompilerId (..))
import Distribution.Parsec
import Distribution.System
import Distribution.Types.Flag (FlagAssignment, FlagName, unFlagName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Utils.Json
import Distribution.Verbosity qualified as Verbosity

import Compat
import CondTree
    ( Env (..)
    , defragC
    , flattenCondTree
    , pushConditionals
    , simplifyGenericPackageDescription
    )
import Distribution.Fields (PrettyField)
import Distribution.Fields.Pretty (CommentPosition (..), PrettyField (..), showFields)
import Distribution.Pretty (Pretty (..))
import Distribution.Simple.Utils (fromUTF8LBS, toUTF8BS)
import Distribution.Types.CondTree (CondBranch (..), CondTree (..))
import Distribution.Types.Condition (Condition (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Dependency (Dependency)
import FieldMap (FieldMap, toList)
import GenericPackageDescription (CondTree', Tree (..), foldTree, runGenericPackageDescription)
import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..))
import Text.PrettyPrint (Doc, char, hsep, parens, text, (<+>))

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

    let trees' :: [(String, Tree (FieldMap (Fragment Json)))]
        trees' =
            -- This is awkward but we are operating on the `a` in `[(String, Tree a)]`
            (fmap . fmap . fmap)
                (fmap (defragC . flattenCondTree) . pushConditionals)
                trees
    -- let
    --     -- y :: [Tree (FieldMap (CondTree ConfVar [Dependency] (NonEmpty (Fragment Json))))]
    --     y :: [(String, Tree (FieldMap (CondTree' (Fragment Json))))]
    --     y = fmap (fmap (fmap pushConditionals)) trees

    -- -- print y
    -- -- -- NOTE: This is the step I do *not* want to make
    -- -- -- y' :: [Tree (FieldMap (Fragment Json)) (FieldMap [Fragment (Cond ConfVar Json)])]
    -- -- -- y' = fmap (second (fmap (foldCondTree (\c -> fmap (fmap (Cond c)))))) y

    -- let y'' :: [(String, Tree (FieldMap (Fragment Json)))]
    --     y'' = fmap (fmap (fmap (fmap (defragC . flattenCondTree)))) y

    putStrLn "trees:"
    putStrLn $
        showFields (const NoComment) $
            foldMap
                ( \(name, tree) ->
                    pure
                        $ PrettySection () (toUTF8BS name) []
                        $ foldTree
                            (ppCondTree2 . fmap (fmap (text . fromUTF8LBS . renderJson . toJSON)) )
                            (\n -> pure . PrettySection () (toUTF8BS n) [])
                        tree
                )
                trees

    putStrLn "trees':"
    putStrLn $
        showFields (const NoComment) $
            foldMap
                ( \(name, tree) ->
                    pure $
                        PrettySection () (toUTF8BS name) [] $
                            foldTree
                                (ppFieldMap . fmap (text . fromUTF8LBS . renderJson . toJSON))
                                (\n -> pure . PrettySection () (toUTF8BS n) [])
                                tree
                )
                trees'

    let json =
            JsonObject $
                mconcat
                    [ [(name, toJSON value) | (name, value) <- FieldMap.toList top]
                    , [(name, toJSON value) | (name, value) <- middle ++ trees']
                    ]

    maybe BL.putStr BL.writeFile optsOutput $ renderJson json

ppFieldMap :: Pretty a => FieldMap a -> [PrettyField ()]
ppFieldMap it = [PrettyField () (toUTF8BS n) (pretty a) | (n, a) <- FieldMap.toList it]

ppCondTree2
    :: (Show a, Pretty a)
    => CondTree ConfVar [Dependency] (FieldMap a)
    -> [PrettyField ()]
ppCondTree2 = go
  where
    go (CondNode it _ ifs) = ppFieldMap it ++ concatMap ppIf ifs

    ppIf (CondBranch c thenTree Nothing)
        --        | isEmpty thenDoc = mempty
        | otherwise = [ppIfCondition c thenDoc]
      where
        thenDoc = go thenTree
    ppIf (CondBranch c thenTree (Just elseTree)) =
        [ ppIfCondition c (go thenTree)
        , PrettySection () "else" [] (go elseTree)
        ]

ppIfCondition
    :: Condition ConfVar
    -> [PrettyField ()]
    -> PrettyField ()
ppIfCondition c =
    PrettySection () "if" [ppCondition c]

ppCondition :: Condition ConfVar -> Doc
ppCondition (Var x) = ppConfVar x
ppCondition (Lit b) = text (show b)
ppCondition (CNot c) = char '!' <> (ppCondition c)
ppCondition (COr c1 c2) =
    parens
        ( hsep
            [ ppCondition c1
            , text "||"
                <+> ppCondition c2
            ]
        )
ppCondition (CAnd c1 c2) =
    parens
        ( hsep
            [ ppCondition c1
            , text "&&"
                <+> ppCondition c2
            ]
        )
ppConfVar :: ConfVar -> Doc
ppConfVar (OS os) = text "os" <> parens (pretty os)
ppConfVar (Arch arch) = text "arch" <> parens (pretty arch)
ppConfVar (PackageFlag name) = text "flag" <> parens (ppFlagName name)
ppConfVar (Impl c v) = text "impl" <> parens (pretty c <+> pretty v)

ppFlagName :: FlagName -> Doc
ppFlagName = text . unFlagName
