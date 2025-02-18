import Data.Traversable (for)

import Distribution.Pretty (prettyShow)
import Distribution.System (OS (..))
import Distribution.Types.Flag (PackageFlag (..), unFlagName)
import Distribution.Types.GenericPackageDescription
import Distribution.Verbosity qualified as Verbosity

import System.FilePath (replaceExtension, (<.>))
import System.Process
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsFile)
import Test.Tasty.HUnit

import Cabal.Syntax.Compat (makeSymbolicPath, readGenericPackageDescription)
import Cabal.Syntax.ListMap qualified as ListMap
import Cabal.Syntax.Utils (These (..), align)

main :: IO ()
main = do
    gt <- goldenTests
    defaultMain $ testGroup "Tests" [unitTests, gt]

testAlign
    :: [(String, Int)]
    -> [(String, Int)]
    -> [(String, These Int Int)]
    -> TestTree
testAlign as bs cs =
    testCase (show cs) $
        assertEqual
            "assert"
            (ListMap.fromList cs)
            (align (ListMap.fromList as) (ListMap.fromList bs))

unitTests :: TestTree
unitTests =
    testGroup
        "unit tests"
        [ testAlign
            [("a", 1), ("b", 2)]
            [("a", 2), ("c", 3)]
            [("a", These 1 2), ("b", This 2), ("c", That 3)]
        , testAlign
            [("a", 1), ("b", 2)]
            [("d", 2)]
            [("a", This 1), ("b", This 2), ("d", That 2)]
        , testAlign
            [("a", 1), ("b", 2), ("c", 3)]
            [("b", 2), ("a", 1)]
            [("a", These 1 1), ("b", These 2 2), ("c", This 3)]
        , testAlign
            [("a", 1), ("b", 2)]
            [("b", 2), ("a", 1), ("c", 3)]
            [("a", These 1 1), ("b", These 2 2), ("c", That 3)]
        , testAlign
            [("a", 1), ("b", 2)]
            []
            [("a", This 1), ("b", This 2)]
        ]

mkTest :: FilePath -> FilePath -> FilePath -> [String] -> TestTree
mkTest name cabalfile ext args =
    goldenVsFile
        name
        goldenfile
        outfile
        (callProcess "cabal2json" ("--out" : outfile : args ++ [cabalfile]))
  where
    goldenfile = replaceExtension cabalfile ext
    outfile = goldenfile <.> "out"

goldenTests :: IO TestTree
goldenTests = do
    cabalfiles <- findByExtension [".cabal"] "tests/golden"
    fmap (testGroup "golden tests") $
        for cabalfiles $ \cabalfile ->
            fmap (testGroup cabalfile) $ do
                let simple = mkTest "original" cabalfile ".json" []

                gpd <- readGenericPackageDescription Verbosity.silent Nothing (makeSymbolicPath cabalfile)

                let test_flags =
                        mconcat
                            [ [ mkTest
                                    (unwords ["flag", pm : flagname])
                                    cabalfile
                                    (".flag" ++ pm : flagname ++ ".json")
                                    ["--flag", pm : flagname]
                              | pm <- "+-"
                              , let flagname = unFlagName (flagName flag)
                              ]
                            | flag <- genPackageFlags gpd
                            ]

                let test_os =
                        [ mkTest
                            (unwords ["os", prettyShow os])
                            cabalfile
                            (".os-" ++ prettyShow os ++ ".json")
                            ["--os", prettyShow os]
                        | os <- [Windows, Linux, OSX]
                        ]

                pure $ simple : test_flags ++ test_os
