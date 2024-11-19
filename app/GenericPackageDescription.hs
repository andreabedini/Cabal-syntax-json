module GenericPackageDescription
    ( runGenericPackageDescription
    , Grouped (..)
    ) where

import Data.Foldable (Foldable (..))

import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.PackageDescription.FieldGrammar
    ( benchmarkFieldGrammar
    , executableFieldGrammar
    , flagFieldGrammar
    , foreignLibFieldGrammar
    , libraryFieldGrammar
    , packageDescriptionFieldGrammar
    , setupBInfoFieldGrammar
    , sourceRepoFieldGrammar
    , testSuiteFieldGrammar
    , unvalidateBenchmark
    , unvalidateTestSuite
    )
import Distribution.Pretty (prettyShow)
import Distribution.Types.CondTree (CondTree (..), mapTreeData)
import Distribution.Types.ConfVar (ConfVar)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Flag (PackageFlag (..), unFlagName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.LibraryName (LibraryName (..), libraryNameString)
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.SetupBuildInfo (SetupBuildInfo)
import Distribution.Types.SourceRepo (SourceRepo (..))
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Json (Json (..))

import Data.Maybe (maybeToList)
import FieldMap (FieldMap)
import JsonFieldGrammar (Fragment (..), JSONFieldGrammar (..), jsonFieldGrammar)
import Json

data Grouped a where
    Top :: a -> Grouped a
    Named :: String -> a -> Grouped a
    Group
        :: String
        -> [(String, a)]
        -> Grouped a
    deriving (Show, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Grouped a) where
    toJSON (Top a) = toJSON a
    toJSON (Named n a) = JsonObject [(n, toJSON a)]
    toJSON (Group n as) = JsonObject [(n, JsonObject [(an, toJSON a) | (an, a) <- as])]

mkCondNode :: Monoid c => a -> CondTree v c a
mkCondNode a = CondNode a mempty mempty

-- | Transform a GenericPackageDescription into our representation.
-- This step already transform types associated with a field grammar into FieldMap (Fragment Json).
-- This is necessary to transform then into a single type.
runGenericPackageDescription
    :: CabalSpecVersion
    -> GenericPackageDescription
    -> [ Grouped
            (CondTree ConfVar [Dependency] (FieldMap (Fragment Json)))
       ]
runGenericPackageDescription v gpd =
    mconcat
        [ [Top $ mkCondNode $ jsonFieldGrammar v packageDescriptionFieldGrammar (packageDescription gpd)]
        , [ Group
                "source-repositories"
                [ ( prettyShow (repoKind repo)
                  , mkCondNode $ jsonFieldGrammar v (sourceRepoFieldGrammar (repoKind repo)) repo
                  )
                | repo <- repos
                ]
          | let repos = sourceRepos (packageDescription gpd)
          , not (null repos)
          ]
        , [ Named "custom-setup" $ mkCondNode $ jsonFgCustomSetup v sbi
          | sbi <- maybeToList (setupBuildInfo (packageDescription gpd))
          ]
        , [ Group
                "flags"
                [ ( unFlagName fn
                  , mkCondNode $ jsonFieldGrammar v (flagFieldGrammar fn) flag
                  )
                | flag <- flags
                , let fn = flagName flag
                ]
          | let flags = genPackageFlags gpd
          , not (null flags)
          ]
        , [ Group
                "libraries"
                [ (libraryName ln, jsonFgCondTree v (libraryFieldGrammar ln) lib)
                | (ln, lib) <- libraries
                ]
          | not (null libraries)
          ]
        , [ Group
                "foreign-libraries"
                [ (unUnqualComponentName name, jsonFgCondTree v (foreignLibFieldGrammar name) flib)
                | (name, flib) <- flibs
                ]
          | let flibs = condForeignLibs gpd
          , not (null flibs)
          ]
        , [ Group
                "executables"
                [ (unUnqualComponentName name, jsonFgCondTree v (executableFieldGrammar name) exe)
                | (name, exe) <- exes
                ]
          | let exes = condExecutables gpd
          , not (null exes)
          ]
        , [ Group
                "test-suites"
                [ (unUnqualComponentName name, jsonFgCondTree v testSuiteFieldGrammar test)
                | (name, test) <- tests
                ]
          | not (null tests)
          ]
        , [ Group
                "benchmarks"
                [ (unUnqualComponentName name, jsonFgCondTree v benchmarkFieldGrammar benchmark)
                | (name, benchmark) <- benchmarks
                ]
          | not (null benchmarks)
          ]
        ]
  where
    pn = pkgName $ package $ packageDescription gpd

    libraryName :: LibraryName -> String
    libraryName = maybe (unPackageName pn) unUnqualComponentName . libraryNameString

    libraries =
        concat
            [ [(LMainLibName, l) | l <- toList (condLibrary gpd)]
            , [(LSubLibName ucn, l) | (ucn, l) <- condSubLibraries gpd]
            ]
    tests =
        [ (ucn, mapTreeData unvalidateTestSuite ts) | (ucn, ts) <- condTestSuites gpd
        ]

    benchmarks =
        [ (ucn, mapTreeData unvalidateBenchmark b)
        | (ucn, b) <- condBenchmarks gpd
        ]

jsonFgCustomSetup :: CabalSpecVersion -> SetupBuildInfo -> FieldMap (Fragment Json)
jsonFgCustomSetup v sbi =
    jsonFieldGrammar v (setupBInfoFieldGrammar False) sbi

jsonFgCondTree
    :: CabalSpecVersion
    -> JSONFieldGrammar a s
    -> CondTree v c a
    -> CondTree v c (FieldMap (Fragment Json))
jsonFgCondTree v fg = mapTreeData (jsonFieldGrammar v fg)
