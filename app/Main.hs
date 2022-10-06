module Main where

import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy.IO as TL
import Distribution.Aeson (jsonGenericPackageDescription)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import qualified Distribution.Verbosity as Verbosity
import System.Environment (getArgs)

main :: IO ()
main = do
  fn : _ <- getArgs
  gpd <- readGenericPackageDescription Verbosity.normal fn
  TL.putStrLn $ encodeToLazyText $ jsonGenericPackageDescription gpd
