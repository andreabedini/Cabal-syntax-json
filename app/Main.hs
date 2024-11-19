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
import Distribution.Types.Flag (FlagAssignment)
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
import FieldMap (FieldMap, toList)
import GenericPackageDescription (CondTree', Tree (..), runGenericPackageDescription)
import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..))

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
        (top,  middle, trees) = runGenericPackageDescription v simplifiedGpd

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

    let json =
            JsonObject $
                mconcat
                    [ [ (name, toJSON value) | (name, value) <- FieldMap.toList top ]
                    , [ (name, toJSON value) | (name, value) <- middle ++ trees' ]
                    ]

    maybe BL.putStr BL.writeFile optsOutput $ renderJson json
