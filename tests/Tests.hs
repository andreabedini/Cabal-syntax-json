import Distribution.Utils.Generic (toUTF8LBS)
import System.FilePath (replaceExtension, takeBaseName)
import System.Process
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

main :: IO ()
main = defaultMain =<< goldenTests

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
