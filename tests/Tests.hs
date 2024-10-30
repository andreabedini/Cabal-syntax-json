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
            "YamlToJson golden tests"
            [ goldenVsString
                (takeBaseName cabalfile)
                jsonFile
                (toUTF8LBS <$> readProcess "cabal2json" [cabalfile] "")
            | cabalfile <- cabalfiles
            , let jsonFile = replaceExtension cabalfile ".json"
            ]
