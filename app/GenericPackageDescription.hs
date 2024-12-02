{-# LANGUAGE FunctionalDependencies #-}

module GenericPackageDescription
    ( runGenericPackageDescription
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
import Distribution.Pretty (prettyShow)
import Distribution.Types.ComponentName
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
import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..), jsonFieldGrammar, jsonFieldGrammar')
import ListMap (ListMap)
import ListMap qualified

type CondTree' a = CondTree ConfVar [Dependency] a

type FieldMap = ListMap String
type ComponentMap = ListMap ComponentName

-- | Transform a GenericPackageDescription into our representation.
-- This step already transform types associated with a field grammar into FieldMap (Fragment Json).
-- This is necessary to transform then into a single type.
runGenericPackageDescription
    :: CabalSpecVersion
    -> GenericPackageDescription
    -> ( (FieldMap Json)
       , ComponentMap
            (CondTree' (FieldMap (Fragment Json)))
       )
runGenericPackageDescription v gpd = (meta, components)
  where
    meta :: ListMap String Json
    meta =
        ListMap.fromList $
            mconcat
                [ -- The output of a field grammar always has unique field names so it can turned into
                  -- field map right away (and soon into a json object)
                  fmap (fmap toJSON) $
                    jsonFieldGrammar v packageDescriptionFieldGrammar (packageDescription gpd)
                , [ ( "custom-setup"
                    , jsonFieldGrammar' v (setupBInfoFieldGrammar False) sbi
                    )
                  | sbi <- maybeToList (setupBuildInfo (packageDescription gpd))
                  ]
                , [ ("source-repositories", sourceRepos2json v repos)
                  | let repos = sourceRepos (packageDescription gpd)
                  , not (null repos)
                  ]
                , [ ("flags", flags2json v flags)
                  | let flags = genPackageFlags gpd
                  , not (null flags)
                  ]
                ]

    components =
        ListMap.fromList $
            fmap (fmap (fmap ListMap.fromList)) $
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

flags2json :: CabalSpecVersion -> [PackageFlag] -> Json
flags2json v flags =
    JsonObject
        [ ( unFlagName (flagName f)
          , jsonFieldGrammar' v (flagFieldGrammar (flagName f)) f
          )
        | f <- flags
        ]

sourceRepos2json :: CabalSpecVersion -> [SourceRepo] -> Json
sourceRepos2json v repos =
    JsonObject
        [ ( prettyShow (repoKind r)
          , jsonFieldGrammar' v (sourceRepoFieldGrammar (repoKind r)) r
          )
        | r <- repos
        ]
