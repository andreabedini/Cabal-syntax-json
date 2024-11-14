{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

import Compat
import CondTree (Cond (..), mkEnv, simplifyGPD, test'V)
import Control.Monad (unless)
import Data.Bifunctor (Bifunctor (..))
import Data.Either
import Data.Foldable
import Data.Foldable1 (Foldable1 (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Distribution.Compiler (CompilerId (..))
import Distribution.PackageDescription hiding (foldCondTree, options)
import Distribution.Parsec
import Distribution.System
import Distribution.Utils.Json
import Distribution.Verbosity (normal)
import GenericPackageDescription (Grouped, runGenericPackageDescription)
import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..))
import MonoidalMap (FieldMap)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)
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
        (ReqArg (either Left (\flags -> Right (\opts -> opts{optsFlags = optsFlags opts <> flags})) . eitherParsec) "FLAG")
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
        x
            :: [ Grouped
                    (FieldMap (Fragment Json))
                    (CondTree ConfVar [Dependency] (FieldMap (Fragment Json)))
               ]
        x = runGenericPackageDescription v gpd

    let x'
            :: [ Grouped
                    (FieldMap (Fragment Json))
                    ( CondTree
                        ConfVar
                        [Dependency]
                        (FieldMap [Fragment Json])
                    )
               ]
        x' = fmap (second (mapTreeData (fmap pure))) x

    let y
            :: [ Grouped
                    (FieldMap (Fragment Json))
                    (FieldMap (CondTree ConfVar [Dependency] [Fragment Json]))
               ]
        y = fmap (second test'V) x'

    -- NOTE: This is the step I do *not* want to make
    -- y' :: [Grouped (FieldMap (Fragment Json)) (FieldMap [Fragment (Cond ConfVar Json)])]
    -- y' = fmap (second (fmap (foldCondTree (\c -> fmap (fmap (Cond c)))))) y

    -- NOTE: now I want to flatten the CondTree
    let y'
            :: [ Grouped
                    (FieldMap (Fragment Json))
                    (FieldMap (Cond ConfVar [Fragment Json]))
               ]
        y' = fmap (second (fmap flatten)) y

    pPrint y'

flatten :: CondTree v c a -> Cond v a
flatten (CondNode a _ ifs) =
    Cond a (foldMap (goBranch (Lit True)) ifs)
  where
    go c (CondNode a' _ ifs') =
        (c, a') : foldMap (goBranch c) ifs'
    goBranch c (CondBranch c' thenTree Nothing) =
        go (c `cAnd` c') thenTree
    goBranch c (CondBranch c' thenTree (Just elseTree)) =
        go (c `cAnd` c') thenTree <> go (c `cAnd` cNot c') elseTree

-- pPrint $ toJSON y

-- let bs = renderJson $ _ x

-- maybe BL.putStr BL.writeFile optsOutput bs

defrag :: [Fragment a] -> Fragment a
defrag = _
