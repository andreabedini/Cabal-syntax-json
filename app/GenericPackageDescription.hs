{-# LANGUAGE FunctionalDependencies #-}

module GenericPackageDescription
    ( runGenericPackageDescription
    , Tree (..)
    , CondTree'
    , foldTree
    , ppTree
    ) where

import Data.Foldable (Foldable (..))
import Data.Maybe (fromMaybe, maybeToList)

import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Fields.Pretty (PrettyField (..))
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
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.SourceRepo (SourceRepo (..))
import Distribution.Types.UnqualComponentName
    ( UnqualComponentName
    , mkUnqualComponentName
    , unUnqualComponentName
    )

import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Distribution.Types.Executable
import Distribution.Types.ForeignLib (ForeignLib)
import Distribution.Types.Library (Library)
import Distribution.Utils.Json (Json (..))
import FieldMap (FieldMap, fromList)
import Json
import JsonFieldGrammar (Fragment (..), JSONFieldGrammar (..), jsonFieldGrammar)
import Pretty (prettySection)

data Tree a where
    Value :: a -> Tree a
    Group :: [(String, Tree a)] -> Tree a
    deriving (Show, Functor, Foldable, Traversable)

-- foldTree :: Monoid m => (a -> m) -> (String -> m -> m) -> Tree a -> m
-- foldTree f _ (Value a) = f a
-- foldTree f g (Group as) = foldMap (\(n, a) -> g n (foldTree f g a)) as

foldTree :: (a -> b) -> ([(String, b)] -> b) -> Tree a -> b
foldTree f _ (Value a) = f a
foldTree f g (Group as) = g [(n, foldTree f g a) | (n, a) <- as]

instance ToJSON a => ToJSON (Tree a) where
    toJSON (Value a) = toJSON a
    toJSON (Group as) = JsonObject [(an, toJSON a) | (an, a) <- as]

type CondTree' a = CondTree ConfVar [Dependency] a

-- data GPD
--     = GPD
--         (FieldMap (Fragment Json))
--         ( Map
--             String
--             ( Map
--                 UnqualComponentName
--                 (CondTree' (Fragment Json))
--             )
--         )

-- | Transform a GenericPackageDescription into our representation.
-- This step already transform types associated with a field grammar into FieldMap (Fragment Json).
-- This is necessary to transform then into a single type.
runGenericPackageDescription
    :: CabalSpecVersion
    -> GenericPackageDescription
    -> ( FieldMap (Fragment Json)
       , Map String (Map UnqualComponentName (CondTree' (FieldMap (Fragment Json))))
       )
runGenericPackageDescription v gpd = (top, bottom)
  where
    top =
        jsonFieldGrammar v packageDescriptionFieldGrammar (packageDescription gpd)
            <> FieldMap.fromList
                [ ( "custom-setup"
                  , ScalarFragment . toJSON $ jsonFieldGrammar v (setupBInfoFieldGrammar False) sbi
                  )
                | sbi <- maybeToList (setupBuildInfo (packageDescription gpd))
                ]
            <> FieldMap.fromList
                [ ( "source-repositories"
                  , ScalarFragment $
                        JsonObject $
                            [ ( prettyShow (repoKind repo)
                              , toJSON $ jsonFieldGrammar v (sourceRepoFieldGrammar (repoKind repo)) repo
                              )
                            | repo <- repos
                            ]
                  )
                | let repos = sourceRepos (packageDescription gpd)
                , not (null repos)
                ]
            <> FieldMap.fromList
                [ ( "flags"
                  , ScalarFragment $
                        JsonObject
                            [ ( unFlagName (flagName flag)
                              , toJSON $ jsonFieldGrammar v (flagFieldGrammar (flagName flag)) flag
                              )
                            | flag <- flags
                            ]
                  )
                | let flags = genPackageFlags gpd
                , not (null flags)
                ]

    bottom :: Map String (Map UnqualComponentName (CondTree' (FieldMap (Fragment Json))))
    bottom =
        Map.fromList $
            mconcat
                [ [ ( "libraries"
                    , Map.fromList
                        [ ( libraryName ln
                          , mapTreeData (jsonFieldGrammar v (libraryFieldGrammar ln)) lib
                          )
                        | (ln, lib) <- libraries
                        ]
                    )
                  | not (null libraries)
                  ]
                , [ ( "foreign-libraries"
                    , Map.fromList
                        [ ( ucn
                          , mapTreeData (jsonFieldGrammar v (foreignLibFieldGrammar ucn)) flib
                          )
                        | (ucn, flib) <- flibs
                        ]
                    )
                  | let flibs = condForeignLibs gpd
                  , not (null flibs)
                  ]
                , [ ( "executables"
                    , Map.fromList
                        [ ( ucn
                          , mapTreeData (jsonFieldGrammar v (executableFieldGrammar ucn)) exe
                          )
                        | (ucn, exe) <- exes
                        ]
                    )
                  | let exes = condExecutables gpd
                  , not (null exes)
                  ]
                , [ ( "test-suites"
                    , Map.fromList
                        [ ( ucn
                          , mapTreeData (jsonFieldGrammar v testSuiteFieldGrammar) test
                          )
                        | (ucn, test) <- tests
                        ]
                    )
                  | not (null tests)
                  ]
                , [ ( "benchmarks"
                    , Map.fromList
                        [ ( ucn
                          , mapTreeData (jsonFieldGrammar v benchmarkFieldGrammar) benchmark
                          )
                        | (ucn, benchmark) <- benchmarks
                        ]
                    )
                  | not (null benchmarks)
                  ]
                ]

    pn = pkgName $ package $ packageDescription gpd

    libraryName :: LibraryName -> UnqualComponentName
    libraryName = fromMaybe (mkUnqualComponentName $ unPackageName pn) . libraryNameString

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

ppTree :: (a -> [PrettyField ()]) -> Tree a -> [PrettyField ()]
ppTree f = foldTree f (map (\(n, t) -> prettySection n [] t))

-- class ToCondTree n a where
--     toCondTree
--         :: CabalSpecVersion
--         -> [(n, CondTree' a)]
--         -> Map UnqualComponentName (CondTree' (FieldMap (Fragment Json)))

-- data Wrapped n a = Wrapped PackageName [(n, CondTree' a)]

-- data Stuff n a = Stuff [(n, CondTree' a)]

-- instance ToCondTree LibraryName Library where
--     toCondTree v libs =
--         Map.fromList
--             [ ( libraryName ln
--               , mapTreeData (jsonFieldGrammar v (libraryFieldGrammar ln)) lib
--               )
--             | (ln, lib) <- libs
--             ]
--       where
--         libraryName :: LibraryName -> UnqualComponentName
--         libraryName = fromMaybe (mkUnqualComponentName $ unPackageName pkgName) . libraryNameString

-- instance ToCondTree UnqualComponentName ForeignLib where
--     toCondTree v flibs =
--         Map.fromList
--             [ ( ucn
--               , mapTreeData (jsonFieldGrammar v (foreignLibFieldGrammar ucn)) flib
--               )
--             | (ucn, flib) <- flibs
--             ]

-- instance ToCondTree UnqualComponentName Executable where
--     toCondTree v flibs =
--         Map.fromList
--             [ ( ucn
--               , mapTreeData (jsonFieldGrammar v (executableFieldGrammar ucn)) flib
--               )
--             | (ucn, flib) <- flibs
--             ]
