{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad (unless, when)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty)
import System.Console.GetOpt
    ( ArgDescr (..)
    , ArgOrder (..)
    , OptDescr (..)
    , getOpt
    , usageInfo
    )
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import Distribution.Compiler (CompilerFlavor (..), CompilerId (..))
import Distribution.Parsec (Parsec, eitherParsec)
import Distribution.System (Arch (..), OS (..), Platform (..))
import Distribution.Types.CondTree qualified as C
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Utils.Generic (toUTF8LBS)
import Distribution.Utils.Json (Json (..), renderJson)
import Distribution.Verbosity qualified as Verbosity

import Data.ByteString.Lazy qualified as BL
import Text.PrettyPrint (render)

import Cabal.Syntax.Compat -- Leave unqualified
import Cabal.Syntax.CondTree
    ( CondTree (..)
    , Guarded
    , convertCondTree
    , defragment
    , flattenCondTree
    , pushConditionals
    )
import Cabal.Syntax.GenericPackageDescription
    ( ComponentMap (..)
    , FieldMap (..)
    , GPD (..)
    , renderFields
    , runGenericPackageDescription
    )
import Cabal.Syntax.Json (ToJSON (..))
import Cabal.Syntax.JsonFieldGrammar (Fragment (..))
import Cabal.Syntax.Pretty (PrettyFieldClass (..))
import Cabal.Syntax.Simplify (Env (..), simplifyGPD)

data Opts = Opts
    { optsArch :: Maybe Arch
    , optsCompiler :: Maybe CompilerId
    , optsFlags :: FlagAssignment
    , optsHelp :: Bool
    , optsOs :: Maybe OS
    , optsOutput :: Maybe FilePath
    , optsDebug :: Bool
    , optsPristine :: Bool
    }
    deriving Show

defaultOptions :: Opts
defaultOptions =
    Opts
        { optsArch = Nothing
        , optsCompiler = Nothing
        , optsFlags = mempty
        , optsHelp = False
        , optsOs = Nothing
        , optsOutput = mempty
        , optsDebug = False
        , optsPristine = False
        }

mkArgDescr :: Parsec a => String -> (a -> b) -> ArgDescr (Either String b)
mkArgDescr placeholder set = ReqArg (fmap set . eitherParsec) placeholder

mkArgDescr' :: Applicative f => String -> (String -> b) -> ArgDescr (f b)
mkArgDescr' placeholder set = ReqArg (fmap set . pure) placeholder

options :: [OptDescr (Either String (Opts -> Opts))]
options =
    [ Option
        "h"
        ["help"]
        (NoArg (Right (\opts -> opts{optsHelp = True})))
        "Print this help message"
    , Option
        ""
        ["out"]
        (mkArgDescr' "FILE" (\fn opts -> opts{optsOutput = Just fn}))
        "Path for output"
    , Option
        "f"
        ["flag"]
        (mkArgDescr "FLAG" (\flags opts -> opts{optsFlags = optsFlags opts <> flags}))
        "Specialise to flag assignment FLAG"
    , Option
        "c"
        ["compiler"]
        (mkArgDescr "COMPILER" (\comp opts -> opts{optsCompiler = Just comp}))
        "Specialise to the compiler COMPILER"
    , Option
        "o"
        ["os"]
        (mkArgDescr "OS" (\os opts -> opts{optsOs = Just os}))
        "Specialise to the operating system OS"
    , Option
        "a"
        ["arch"]
        (mkArgDescr "ARCH" (\arch opts -> opts{optsArch = Just arch}))
        "Specialise to the architecture ARCH"
    , Option
        "p"
        ["platform"]
        (mkArgDescr "PLATFORM" (\(Platform arch os) opts -> opts{optsArch = Just arch, optsOs = Just os}))
        "Specialise to the platform PLATFORM"
    , Option
        ""
        ["debug"]
        (NoArg (Right (\opts -> opts{optsDebug = True})))
        "Output an intermediate format useful for debugging"
    , Option
        ""
        ["pristine"]
        (NoArg (Right (\opts -> opts{optsPristine = True})))
        "Preserve the conditional structure"
    ]

header :: String
header = "Usage: cabal2json [OPTION...] files..."

putWarn :: String -> IO ()
putWarn = hPutStrLn stderr . ("Warning: " ++)

main :: IO ()
main = do
    (o, as, errs0) <- getOpt RequireOrder options <$> getArgs
    let (errs1, o') = partitionEithers o
        opts = foldl' (flip id) defaultOptions o'

        -- errs0 are reported by getOpt and they include things like missing
        -- arguments or unknown flags.
        -- errs1 are instead the result of parsing the option arguments.
        errs = errs0 ++ errs1

    when (optsHelp opts) $ do
        putStrLn $ usageInfo header options
        exitSuccess

    unless (null errs) $ do
        putStrLn $ usageInfo (unlines (header : "" : errs)) options
        exitFailure

    case (opts, as) of
        (_, []) -> do
            putStrLn $ usageInfo (unlines [header, "", "No files provided."]) options
            exitFailure
        (_, [fn_in]) ->
            main' opts fn_in
        (Opts{optsOutput = Nothing}, paths) ->
            for_ paths $ main' opts
        (Opts{optsOutput = Just _}, _ : _ : _) -> do
            putStrLn "Output to file is incompatible with multiple inputs"
            exitFailure

main' :: Opts -> FilePath -> IO ()
main' Opts{..} fn = do
    case optsArch of
        Just (OtherArch a) -> putWarn $ "Specialising to unknown architecture: " ++ a ++ "."
        _ -> pure ()

    case optsCompiler of
        Just (CompilerId (OtherCompiler c) _) -> putWarn $ "Specialising to unknown compiler: " ++ c ++ "."
        _ -> pure ()

    case optsOs of
        Just (OtherOS os) -> putWarn $ "Specialising to unknown operating system: " ++ os ++ "."
        _ -> pure ()

    gpd <- readGenericPackageDescription Verbosity.silent Nothing (makeSymbolicPath fn)

    let env = Env optsOs optsArch optsCompiler optsFlags
        GPD top components0 =
            runGenericPackageDescription
                (specVersion (packageDescription gpd))
                (simplifyGPD env gpd)

    let format :: (ToJSON b, PrettyFieldClass b) => GPD (Fragment Json) b -> BL.ByteString
        format = if optsDebug then formatPretty else formatJson

    let writeOut :: BL.ByteString -> IO ()
        writeOut = maybe BL.putStr BL.writeFile optsOutput

    writeOut $
        if optsPristine
            then
                format $ GPD top components0
            else
                format $ GPD top (process components0)

formatPretty :: PrettyFieldClass b => GPD (Fragment Json) b -> BL.ByteString
formatPretty =
    toUTF8LBS . render . renderFields . prettyField

formatJson :: (ToJSON a, ToJSON b) => GPD a b -> BL.ByteString
formatJson =
    renderJson . toJSON

process
    :: ComponentMap (C.CondTree ConfVar c (FieldMap (Fragment Json)))
    -> ComponentMap (FieldMap (Fragment Json))
process components0 = components4
  where
    components1 :: ComponentMap (CondTree ConfVar (FieldMap (Fragment Json)))
    components1 = fmap convertCondTree components0

    components2 :: ComponentMap (FieldMap (CondTree ConfVar (Fragment Json)))
    components2 = fmap pushConditionals components1

    components3 :: ComponentMap (FieldMap (NonEmpty (Guarded ConfVar (Fragment Json))))
    components3 = fmap (fmap flattenCondTree) components2

    components4 :: ComponentMap (FieldMap (Fragment Json))
    components4 = fmap (fmap defragment) components3
