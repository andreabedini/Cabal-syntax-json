{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad (unless)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import System.Console.GetOpt
    ( ArgDescr (..)
    , ArgOrder (..)
    , OptDescr (..)
    , getOpt
    , usageInfo
    )
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Distribution.Compiler (CompilerId (..))
import Distribution.Fields.Pretty (CommentPosition (..), showFields)
import Distribution.Parsec (Parsec, eitherParsec)
import Distribution.System (Arch (..), OS (..), Platform (..))
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Utils.Json (Json (..), renderJson, (.=))
import Distribution.Verbosity qualified as Verbosity

import Data.ByteString.Lazy qualified as BL
import Distribution.Pretty (Pretty (..))

import Compat (makeSymbolicPath, readGenericPackageDescription)
import CondTree
    ( Cond
    , Env (..)
    , MyCondTree' (..)
    , banner
    , convertCondTree'
    , flattenCondTree'
    , pushConditionals'
    , simplifyGenericPackageDescription
    )
import FieldMap (FieldMap (..), toList)
import GenericPackageDescription
    ( Components (..)
    , GPD (..)
    , foldComponents
    , runGenericPackageDescription
    )
import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..))
import Pretty (prettyField)

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
        components0
            :: Components
                (CondTree ConfVar [Dependency] (FieldMap (Fragment Json)))
        GPD top components0 = runGenericPackageDescription v simplifiedGpd

    putStrLn "original"
    putStrLn $ showFields (const NoComment) $ prettyField components0

    let components1 :: Components (MyCondTree' ConfVar (FieldMap (Fragment Json)))
        components1 = fmap convertCondTree' components0
    putStrLn (banner "converted")
    putStrLn $ showFields (const NoComment) $ prettyField components1

    let components2 :: Components (FieldMap (MyCondTree' ConfVar (Fragment Json)))
        components2 = fmap pushConditionals' components1
    putStrLn (banner "pushed")
    print $ pretty components2

    let components3 :: Components (FieldMap (Cond ConfVar (Fragment Json)))
        components3 = fmap (fmap flattenCondTree') components2
    putStrLn (banner "flattened")
    print $ pretty components3

    -- let json = toJSON components3

    let json =
            JsonObject $
                FieldMap.toList (fmap toJSON top)
                    <> foldComponents
                        (\libs -> [("libraries" .= toJSON libs) | not (null libs)])
                        (\flibs -> [("foreign-libraries" .= toJSON flibs) | not (null flibs)])
                        (\exes -> [("executables" .= toJSON exes) | not (null exes)])
                        (\tests -> [("test-suites" .= toJSON tests) | not (null tests)])
                        (\benchs -> [("benchmarks" .= toJSON benchs) | not (null benchs)])
                        components3

    -- let json =
    --         JsonObject $
    --             mconcat
    --                 [ [(name, toJSON value) | (name, value) <- FieldMap.toList top]
    --                 , [(name, toJSON value) | (name, value) <- middle ++ components2]
    --                 ]

    maybe BL.putStr BL.writeFile optsOutput $ renderJson json
