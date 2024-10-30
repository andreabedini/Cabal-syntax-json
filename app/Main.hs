import Compat
import Control.Monad (unless)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable
import Distribution.Utils.Json
import Distribution.Verbosity (normal)
import FieldGrammar
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)

data Opts = Opts {optsOutput :: Maybe FilePath}

defaultOptions :: Opts
defaultOptions = Opts{optsOutput = Nothing}

options :: [OptDescr (Opts -> Opts)]
options =
    [ Option "o" ["out"] (ReqArg (\s opts -> opts{optsOutput = Just s}) "FILE") "Path for output"
    ]

header :: String
header = "Usage: cabal2sync [OPTION...] files..."

main :: IO ()
main = do
    (o, as, errs) <- getOpt RequireOrder options <$> getArgs
    unless (null errs) $ do
        putStrLn $ concat errs ++ usageInfo header options
        exitFailure
    let opts = foldl' (flip id) defaultOptions o
    case (opts, as) of
        (_, []) -> do
            putStrLn "No files provided"
            exitFailure
        (Opts{optsOutput}, [fn_in]) -> do
            gpd <- readGenericPackageDescription normal Nothing (makeSymbolicPath fn_in)
            let bs = renderJson $ jsonGenericPackageDescription gpd
            maybe BL.putStr BL.writeFile optsOutput bs
        (Opts{optsOutput = Nothing}, paths) -> for_ paths $ \fn_in -> do
            gpd <- readGenericPackageDescription normal Nothing (makeSymbolicPath fn_in)
            let bs = renderJson $ jsonGenericPackageDescription gpd
            BL.putStr bs
        (Opts{optsOutput = Just _}, _ : _ : _) -> do
            putStrLn "Output to file is incompatible with multiple inputs"
            exitFailure
