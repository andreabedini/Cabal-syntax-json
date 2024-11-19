{-# LANGUAGE RecordWildCards #-}

import Compat
import CondTree (Cond (..), flattenCondTree, mkEnv, pushConditionals, simplifyGPD)
import Control.Monad (unless)
import Data.ByteString.Lazy qualified as BL
import Data.Either
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Semigroup (..))
import Distribution.Compiler (CompilerId (..))
import Distribution.PackageDescription hiding (foldCondTree, options)
import Distribution.Parsec
import Distribution.System
import Distribution.Utils.Json
import Distribution.Verbosity (normal)
import FieldMap (FieldMap)
import GenericPackageDescription (Grouped (..), runGenericPackageDescription)
import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..))
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)

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
    let env = mkEnv optsOs optsArch optsCompiler optsFlags
    gpd <- simplifyGPD env <$> readGenericPackageDescription normal Nothing (makeSymbolicPath fn)

    let v = specVersion (packageDescription gpd)
        x :: [Grouped (CondTree ConfVar [Dependency] (FieldMap (Fragment Json)))]
        x = runGenericPackageDescription v gpd

    let x' :: [Grouped (CondTree ConfVar [Dependency] (FieldMap (NonEmpty (Fragment Json))))]
        x' = fmap (fmap $ mapTreeData (fmap NE.singleton)) x

    let -- y :: [Grouped (FieldMap (CondTree ConfVar [Dependency] (NonEmpty (Fragment Json))))]
        y :: [Grouped (FieldMap (CondTree ConfVar [Dependency] (Fragment Json)))]
        y = fmap (fmap pushConditionals) x

    -- print y
    -- -- NOTE: This is the step I do *not* want to make
    -- -- y' :: [Grouped (FieldMap (Fragment Json)) (FieldMap [Fragment (Cond ConfVar Json)])]
    -- -- y' = fmap (second (fmap (foldCondTree (\c -> fmap (fmap (Cond c)))))) y

    let y'' :: [Grouped (FieldMap (Fragment Json))]
        y'' = fmap (fmap (fmap (defragC . flattenCondTree))) y
        
    maybe BL.putStr BL.writeFile optsOutput $ renderJson $ toJSON y''


jsonCond :: ToJSON a => (a, Fragment Json) -> Json
jsonCond (a, b) = JsonObject ["_if" .= toJSON a, "_then" .= toJSON b]

defragC :: Cond ConfVar (Fragment Json) -> Fragment Json
defragC (Cond (ScalarFragment a) cs) =
    case NE.nonEmpty cs of
        Nothing -> ScalarFragment a
        Just cs' -> ListLikeFragment (a `NE.cons` NE.map jsonCond cs')
defragC (Cond (ListLikeFragment as) cs) =
    case NE.nonEmpty cs of
        Nothing -> ListLikeFragment as
        Just cs' -> ListLikeFragment (as <> NE.map jsonCond cs')

-- bifoldG :: ToJSON a => Grouped a a -> [(String, Json)]
-- bifoldG (Entry n a) = [(n, toJSON a)]
-- bifoldG (EntryC n a) = [(n, toJSON a)]
-- bifoldG (Group n gs) = [(n, JsonObject $ foldMap bifoldG gs)]
