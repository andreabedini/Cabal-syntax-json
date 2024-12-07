import Distribution.Utils.Generic (toUTF8LBS)
import System.FilePath (replaceExtension, takeBaseName)
import System.Process
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.HUnit

import ListMap qualified
import These (These (..), align)

main :: IO ()
main = do
    gt <- goldenTests
    defaultMain $ testGroup "Tests" [unitTests, gt]

testAlign
    :: (Eq a, Eq b, Show a, Show b, Semigroup b, Semigroup a)
    => String
    -> [(String, a)]
    -> [(String, b)]
    -> [(String, These a b)]
    -> TestTree
testAlign t as bs cs =
    testCase t $
        assertEqual
            "assert"
            (ListMap.fromList cs)
            (align (ListMap.fromList as) (ListMap.fromList bs))

unitTests :: TestTree
unitTests =
    testGroup
        "unit tests"
        [ testAlign @[Int] @[Int]
            "a"
            [("a", [1]), ("b", [2])]
            [("a", [2]), ("c", [3])]
            [("a", These [1] [2]), ("b", This [2]), ("c", That [3])]
        , testAlign @[Int] @[Int]
            "b"
            [("a", [1]), ("b", [2])]
            [("d", [2])]
            [("a", This [1]), ("b", This [2]), ("d", That [2])]
        , testAlign @[Int] @[Int]
            "c"
            [("a", [1]), ("b", [2]), ("c", [3])]
            [("b", [2]), ("a", [1])]
            [("a", These [1] [1]), ("b", These [2] [2]), ("c", This [3])]
        , testAlign @[Int] @[Int]
            "c"
            [("a", [1]), ("b", [2])]
            [("b", [2]), ("a", [1]), ("c", [3])]
            [("a", These [1] [1]), ("b", These [2] [2]), ("c", That [3])]
        , testAlign @[Int] @[Int]
            "d"
            [("a", [1]), ("b", [2])]
            []
            [("a", This [1]), ("b", This [2])]
        ]

goldenTests :: IO TestTree
goldenTests = do
    cabalfiles <- findByExtension [".cabal"] "tests/golden"
    return $
        testGroup
            "golden tests"
            [ testGroup
                "simple"
                [ goldenVsString
                    (takeBaseName cabalfile)
                    jsonFile
                    (toUTF8LBS <$> readProcess "cabal2json" [cabalfile] "")
                | cabalfile <- cabalfiles
                , let jsonFile = replaceExtension cabalfile ".json"
                ]
            , testGroup
                "flags"
                [ goldenVsString
                    (takeBaseName cabalfile)
                    jsonFile
                    (toUTF8LBS <$> readProcess "cabal2json" ["-f", "+os-string", cabalfile] "")
                | cabalfile <- cabalfiles
                , let jsonFile = replaceExtension cabalfile ".flag-os-string.json"
                ]
            , testGroup
                "os"
                [ goldenVsString
                    (takeBaseName cabalfile)
                    jsonFile
                    (toUTF8LBS <$> readProcess "cabal2json" ["--os", "windows", cabalfile] "")
                | cabalfile <- cabalfiles
                , let jsonFile = replaceExtension cabalfile ".os-windows.json"
                ]
            ]
