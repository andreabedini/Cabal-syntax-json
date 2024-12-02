{-# LANGUAGE FunctionalDependencies #-}

module GenericPackageDescription
    ( runGenericPackageDescription
    , GPD (..)
    , CondTree'
    , MyCondTree (..)
    , MyCondBranch (..)
    ) where

import Data.Foldable (Foldable (..))
import Data.Maybe (maybeToList)

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
import Distribution.Types.CondTree (CondTree)
import Distribution.Types.ConfVar (ConfVar)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Flag (PackageFlag (..), unFlagName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.SourceRepo (SourceRepo (..))
import Distribution.Utils.Json (Json (..))

import CondTree (MyCondBranch (..), MyCondTree (..))
import Distribution.Pretty (prettyShow)
import Distribution.Types.ComponentName
import FieldMap (FieldMap, fromList, union)
import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..), JSONFieldGrammar, jsonFieldGrammar)

data GPD a b = GPD a [(ComponentName, b)]

type CondTree' a = CondTree ConfVar [Dependency] a

-- | Transform a GenericPackageDescription into our representation.
-- This step already transform types associated with a field grammar into FieldMap (Fragment Json).
-- This is necessary to transform then into a single type.
runGenericPackageDescription
    :: CabalSpecVersion
    -> GenericPackageDescription
    -> GPD
        (FieldMap Json)
        (CondTree' (FieldMap (Fragment Json)))
runGenericPackageDescription v gpd = GPD meta components
  where
    -- The output of a field grammar always has unique field names so it can turned into
    -- field map right away (and soon into a json object)
    pd = jsonFieldGrammar v packageDescriptionFieldGrammar (packageDescription gpd)

    meta =
        FieldMap.union
            (fmap toJSON pd)
            ( FieldMap.fromList $
                mconcat
                    [ [ ( "custom-setup"
                        , toJSON (jsonFieldGrammar v (setupBInfoFieldGrammar False) sbi)
                        )
                      | sbi <- maybeToList (setupBuildInfo (packageDescription gpd))
                      ]
                    , [ ( "source-repositories"
                        , mkJsonObject v (prettyShow . repoKind) (sourceRepoFieldGrammar . repoKind) repos
                        )
                      | let repos = sourceRepos (packageDescription gpd)
                      , not (null repos)
                      ]
                    , [ ( "flags"
                        , mkJsonObject v (unFlagName . flagName) (flagFieldGrammar . flagName) flags
                        )
                      | let flags = genPackageFlags gpd
                      , not (null flags)
                      ]
                    ]
            )

    components =
        mconcat
            [ [ (CLibName n, fmap (jsonFieldGrammar v (libraryFieldGrammar n)) c)
              | (n, c) <- libs
              ]
            , [ (CFLibName n, fmap (jsonFieldGrammar v (foreignLibFieldGrammar n)) c)
              | (n, c) <- condForeignLibs gpd
              ]
            , [ (CExeName n, fmap (jsonFieldGrammar v (executableFieldGrammar n)) c)
              | (n, c) <- condExecutables gpd
              ]
            , [ (CTestName n, fmap (jsonFieldGrammar v testSuiteFieldGrammar) c)
              | (n, c) <- tests
              ]
            , [ (CBenchName n, fmap (jsonFieldGrammar v benchmarkFieldGrammar) c)
              | (n, c) <- benchmarks
              ]
            ]

    -- pn = pkgName $ package $ packageDescription gpd

    -- libraryName = fromMaybe (mkUnqualComponentName $ unPackageName pn) . libraryNameString

    libs =
        concat
            [ [(LMainLibName, l) | l <- toList (condLibrary gpd)]
            , [(LSubLibName ucn, l) | (ucn, l) <- condSubLibraries gpd]
            ]

    tests =
        [ (ucn, fmap unvalidateTestSuite ts)
        | (ucn, ts) <- condTestSuites gpd
        ]

    benchmarks =
        [ (ucn, fmap unvalidateBenchmark b)
        | (ucn, b) <- condBenchmarks gpd
        ]

mkJsonObject
    :: CabalSpecVersion
    -> (s -> String)
    -> (s -> JSONFieldGrammar s a)
    -> [s]
    -> Json
mkJsonObject v mkName fg as =
    JsonObject
        [ (mkName a, toJSON $ jsonFieldGrammar v (fg a) a)
        | a <- as
        ]
