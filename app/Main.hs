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
import Distribution.Parsec (Parsec, eitherParsec)
import Distribution.Pretty (Pretty (..), prettyShow)
import Distribution.System (Arch (..), OS (..), Platform (..))
import Distribution.Types.ComponentName
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Utils.Generic (fromUTF8LBS, toUTF8BS, toUTF8LBS)
import Distribution.Utils.Json (Json (..), renderJson, (.=))
import Distribution.Verbosity qualified as Verbosity

import Data.ByteString.Lazy qualified as BL

import Compat (makeSymbolicPath, readGenericPackageDescription)
import CondTree
    ( Cond
    , Env (..)
    , MyCondTree (..)
    , convertCondTree
    , defragC
    , flattenCondTree
    , pushConditionals
    , simplifyGenericPackageDescription
    )

import Distribution.Fields.Pretty (CommentPosition (..), PrettyField (..), showFields)
import GenericPackageDescription
    ( CondTree'
    , runGenericPackageDescription
    )
import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..))
import ListMap (ListMap, singleton, toList, union)
import Text.PrettyPrint (Doc, render, text, ($$))

data Opts = Opts
    { optsOutput :: Maybe FilePath
    , optsFlags :: FlagAssignment
    , optsArch :: Maybe Arch
    , optsOs :: Maybe OS
    , optsCompiler :: Maybe CompilerId
    , optsPretty :: Bool
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
        , optsPretty = False
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
        ""
        ["pretty"]
        (NoArg (Right (\opts -> opts{optsPretty = True})))
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
        top :: ListMap String Json
        components0 :: ListMap ComponentName (CondTree' (ListMap String (Fragment Json)))
        (top, components0) = runGenericPackageDescription v simplifiedGpd

    let components1 :: ListMap ComponentName (MyCondTree ConfVar (ListMap String (Fragment Json)))
        components1 = fmap convertCondTree components0

    let components2 :: ListMap ComponentName (ListMap String (MyCondTree ConfVar (Fragment Json)))
        components2 = fmap pushConditionals components1

    let components3 :: ListMap ComponentName (ListMap String (Cond ConfVar (Fragment Json)))
        components3 = fmap (fmap flattenCondTree) components2

    let components4 :: ListMap ComponentName (ListMap String Json)
        components4 = fmap (fmap defragC) components3

    let output =
            if optsPretty
                then
                    toUTF8LBS $
                        render $
                            pretty (fmap (text . fromUTF8LBS . renderJson) top)
                                $$ prettyComponents (ListMap.toList components4)
                else
                    renderJson
                        $ toJSON
                        $ ListMap.union
                            (fmap toJSON top)
                        $ ListMap.singleton
                            "components"
                        $ components2json (ListMap.toList components4)

    maybe BL.putStr BL.writeFile optsOutput output

components2json :: ToJSON a => [(ComponentName, ListMap String a)] -> Json
components2json cs =
    JsonObject
        [ name .= toJSON c
        | (cn, c) <- cs
        , let name = prettyShow cn
        ]

prettyComponents :: ToJSON a => [(ComponentName, ListMap String a)] -> Doc
prettyComponents cs =
    text $
        showFields
            (const NoComment)
            [ PrettyField () (toUTF8BS name) (pretty comp)
            | (cn, c) <- cs
            , let name = prettyShow cn
                  comp = fmap (text . fromUTF8LBS . renderJson . toJSON) c
            ]
