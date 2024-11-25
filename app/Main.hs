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

import Data.Map qualified as Map
import Text.PrettyPrint (Doc, text)

import Distribution.Compat.NonEmptySet (fromNonEmpty)
import Distribution.Compiler (CompilerId (..))
import Distribution.Fields (PrettyField)
import Distribution.Fields.Pretty (CommentPosition (..), showFields)
import Distribution.Parsec
import Distribution.Pretty (Pretty (..))
import Distribution.Simple.Utils (fromUTF8LBS)
import Distribution.System (Arch (..), OS (..), Platform (..))
import Distribution.Types.CondTree (CondBranch (..), CondTree (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.UnqualComponentName (UnqualComponentName, unUnqualComponentName)
import Distribution.Types.Version (mkVersion)
import Distribution.Types.VersionRange (earlierVersion)
import Distribution.Utils.Json
import Distribution.Verbosity qualified as Verbosity

import Compat
import CondTree
    ( Env (..)
    , MyCondBranch (..)
    , MyCondTree (..)
    -- , pushConditionals

    , convertCondTree'
    , pushConditionalsOld
    , simplifyGenericPackageDescription
    )
import Data.These (These (..))
import Distribution.Types.Condition (Condition (..))
import Distribution.Types.Dependency (Dependency)
import FieldMap (FieldMap (..), fromList, toList)
import GenericPackageDescription
    ( Components (..)
    , GPD (..)
    , MyCondTree
    , Tree (..)
    , foldTree
    , runGenericPackageDescription
    )
import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..))
import Pretty (prettyField, prettySection)
import Text.Pretty.Simple (pPrint)

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
        components :: Components (MyCondTree ConfVar (FieldMap (Fragment Json)))
        GPD top components = runGenericPackageDescription v simplifiedGpd

    putStrLn "original"
    putStrLn $
        showFields (const NoComment) $
            prettyComponents
                ( \k c ->
                    [ prettySection (unUnqualComponentName k) [] $
                        prettyField (fmap (fmap something) c)
                    ]
                )
                components

    -- let components0 :: Components (MyCondTree ConfVar (FieldMap (NE.NonEmpty (Fragment Json))))
    --     components0 = (fmap . fmap . fmap) NE.singleton components

    -- putStrLn "wrap in NonEmpty"
    -- putStrLn $
    --     showFields (const NoComment) $
    --         prettyComponents
    --             ( \k c ->
    --                 [ prettySection (unUnqualComponentName k) [] $
    --                     ppCondTree2 (ppFieldMap pretty) (fmap (fmap something) c)
    --                 ]
    --             )
    --             components0

    -- let components1 :: Components (FieldMap (MyCondTree ConfVar (NE.NonEmpty (Fragment Json))))
    --     components1 = fmap pushConditionals components0

    -- putStrLn "after pushConditionals"
    -- pPrint components1

    -- putStrLn $
    --     showFields (const NoComment) $
    --         prettyComponents
    --             ( \k c ->
    --                 ppFieldMap
    --                     (text . showFields (const NoComment) . ppCondTree2 _)
    --                     c
    --                     -- [ prettySection (unUnqualComponentName k) [] $
    --                     --     ppCondTree2 (fmap (fmap something) c)
    --                     -- ]
    --             )
    --             components1
    -- pp
    --     (\fm -> [prettyField n (something $ condTreeJson a) | (n, a) <- FieldMap.toList fm])
    --     components1

    -- let components2 :: [(String, Tree (FieldMap (Cond ConfVar (Fragment Json))))]
    --     components2 = (fmap . fmap . fmap) (fmap flattenCondTree) components1

    -- putStrLn "after flattenCondTree"
    -- putStrLn $
    --     pp (\fm -> [prettyField n (something a) | (n, a) <- FieldMap.toList fm]) components2

    -- let components3 :: [(String, Tree (FieldMap (Fragment Json)))]
    --     components3 = (fmap . fmap . fmap) (fmap defragC) components2

    -- putStrLn "components':"
    -- putStrLn $ pp2 components3

    -- let json =
    --         JsonObject $
    --             mconcat
    --                 [ [(name, toJSON value) | (name, value) <- FieldMap.toList top]
    --                 , [(name, toJSON value) | (name, value) <- middle ++ components']
    --                 ]

    -- maybe BL.putStr BL.writeFile optsOutput $ renderJson json
    putStrLn "Maybe it works"

prettyComponents
    :: (UnqualComponentName -> a -> [PrettyField ()])
    -> Components a
    -> [PrettyField ()]
prettyComponents f (Components libs flibs exes tests benchs) =
    [ prettySection "libraries" [] (Map.foldMapWithKey f libs)
    , prettySection "foreign-libraries" [] (Map.foldMapWithKey f flibs)
    , prettySection "executables" [] (Map.foldMapWithKey f exes)
    , prettySection "test-suites" [] (Map.foldMapWithKey f tests)
    , prettySection "benchmarks" [] (Map.foldMapWithKey f benchs)
    ]

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

-- pp1 :: [(String, Tree (CondTree' (FieldMap (Fragment Json))))] -> String
-- pp1 = pp (ppCondTree2 . fmap (fmap something))

-- pp2 :: ToJSON a => [(String, Tree (FieldMap a))] -> String
-- pp2 = pp (ppFieldMap . fmap something)

something :: ToJSON a => a -> Doc
something = text . fromUTF8LBS . renderJson . toJSON

test :: MyCondTree ConfVar (FieldMap (NE.NonEmpty (Fragment Json)))
test =
    MyCondNode
        { myCondTreeData = mempty
        , myCondTreeComponents =
            [ MyCondBranch
                { myCondBranchCondition = CNot (Var (OS Windows))
                , myCondBranchOptions =
                    This $
                        MyCondNode
                            { myCondTreeData = FieldMap.fromList [("buildable", ScalarFragment (JsonBool False) NE.:| [])]
                            , myCondTreeComponents = []
                            }
                }
            ]
        }

-- test1 :: FieldMap (MyCondTree ConfVar (NE.NonEmpty (Fragment Json)))
-- test1 = pushConditionals test

testOld :: CondTree ConfVar [Dependency] (FieldMap (NE.NonEmpty (Fragment Json)))
testOld =
    CondNode
        { condTreeData = mempty
        , condTreeConstraints = []
        , condTreeComponents =
            [ CondBranch
                { condBranchCondition = CNot (Var (OS Windows))
                , condBranchIfTrue =
                    CondNode
                        { condTreeData = FieldMap.fromList [("buildable", ScalarFragment (JsonBool False) NE.:| [])]
                        , condTreeConstraints = []
                        , condTreeComponents = []
                        }
                , condBranchIfFalse = Nothing
                }
            ]
        }

testOld1 :: FieldMap (CondTree ConfVar [Dependency] (NE.NonEmpty (Fragment Json)))
testOld1 = pushConditionalsOld testOld

testOld2 = convertCondTree' testOld
