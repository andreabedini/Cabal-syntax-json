{-# LANGUAGE FunctionalDependencies #-}

module GenericPackageDescription
    ( runGenericPackageDescription
    , GPD (..)
    , Components (..)
    , MyCondTree (..)
    , MyCondBranch (..)
    , foldComponents
    ) where

import Data.Foldable (Foldable (..))
import Data.Maybe (fromMaybe, maybeToList)

import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Fields.Pretty (CommentPosition (..), PrettyField, showFields)
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
import Distribution.Pretty (Pretty (..), prettyShow)
import Distribution.Types.CondTree (CondTree)
import Distribution.Types.ConfVar (ConfVar)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Flag (PackageFlag (..), unFlagName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.LibraryName (LibraryName (..), libraryNameString)
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.SourceRepo (SourceRepo (..))
import Distribution.Types.UnqualComponentName
    ( UnqualComponentName
    , mkUnqualComponentName
    , unUnqualComponentName
    )
import Distribution.Utils.Json (Json (..), (.=))

import Data.Map (Map)
import Data.Map.Strict qualified as Map

import CondTree (MyCondBranch (..), MyCondTree (..))
import FieldMap (FieldMap, fromList)
import Json
import JsonFieldGrammar (Fragment (..), jsonFieldGrammar)
import Pretty (PrettyFieldClass (..), prettySection)
import Text.PrettyPrint (text)

data GPD a b = GPD a (Components b)
type GPD' =
    GPD (FieldMap (Fragment Json)) (CondTree ConfVar [Dependency] (FieldMap (Fragment Json)))

data Components a = Components
    { compLibraries :: Map UnqualComponentName a
    , compForeignLibraries :: Map UnqualComponentName a
    , compExecutables :: Map UnqualComponentName a
    , compTestSuites :: Map UnqualComponentName a
    , compBenchmarks :: Map UnqualComponentName a
    }
    deriving (Show, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Components a) where
    toJSON (Components libs flibs exes tests benchs) =
        JsonObject $
            mconcat
                [ [("libraries" .= toJSON libs) | not (null libs)]
                , [("foreign-libraries" .= toJSON flibs) | not (null flibs)]
                , [("executables" .= toJSON exes) | not (null exes)]
                , [("test-suites" .= toJSON tests) | not (null tests)]
                , [("benchmarks" .= toJSON benchs) | not (null benchs)]
                ]

foldComponents
    :: Semigroup m
    => (Map UnqualComponentName a -> m)
    -> (Map UnqualComponentName a -> m)
    -> (Map UnqualComponentName a -> m)
    -> (Map UnqualComponentName a -> m)
    -> (Map UnqualComponentName a -> m)
    -> Components a
    -> m
foldComponents f1 f2 f3 f4 f5 (Components lib flib exe test bench) =
    f1 lib <> f2 flib <> f3 exe <> f4 test <> f5 bench

instance PrettyFieldClass a => PrettyFieldClass (Components a) where
    prettyField (Components libs flibs exes tests benchs) =
        mconcat
            [ [prettySection "libraries" [] (Map.foldMapWithKey f libs) | not (null libs)]
            , [prettySection "foreign-libraries" [] (Map.foldMapWithKey f flibs) | not (null flibs)]
            , [prettySection "executables" [] (Map.foldMapWithKey f exes) | not (null exes)]
            , [prettySection "test-suites" [] (Map.foldMapWithKey f tests) | not (null tests)]
            , [prettySection "benchmarks" [] (Map.foldMapWithKey f benchs) | not (null benchs)]
            ]
      where
        f :: PrettyFieldClass b => UnqualComponentName -> b -> [PrettyField ()]
        f k c = [prettySection (unUnqualComponentName k) [] (prettyField c)]

instance PrettyFieldClass a => Pretty (Components a) where
    -- LOL but works
    pretty = text . showFields (const NoComment) . prettyField

-- something = text . fromUTF8LBS . renderJson . toJSON

-- | Transform a GenericPackageDescription into our representation.
-- This step already transform types associated with a field grammar into FieldMap (Fragment Json).
-- This is necessary to transform then into a single type.
runGenericPackageDescription
    :: CabalSpecVersion
    -> GenericPackageDescription
    -> GPD'
runGenericPackageDescription v gpd = GPD top bottom
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

    bottom =
        Components
            { compLibraries =
                Map.fromList
                    [ ( libraryName ln
                      , fmap (jsonFieldGrammar v (libraryFieldGrammar ln)) lib
                      )
                    | (ln, lib) <- libraries
                    ]
            , compForeignLibraries =
                Map.fromList
                    [ ( ucn
                      , fmap (jsonFieldGrammar v (foreignLibFieldGrammar ucn)) flib
                      )
                    | (ucn, flib) <- condForeignLibs gpd
                    ]
            , compExecutables =
                Map.fromList
                    [ ( ucn
                      , fmap (jsonFieldGrammar v (executableFieldGrammar ucn)) exe
                      )
                    | (ucn, exe) <- condExecutables gpd
                    ]
            , compTestSuites =
                Map.fromList
                    [ ( ucn
                      , fmap (jsonFieldGrammar v testSuiteFieldGrammar) test
                      )
                    | (ucn, test) <- tests
                    ]
            , compBenchmarks =
                Map.fromList
                    [ ( ucn
                      , fmap (jsonFieldGrammar v benchmarkFieldGrammar) benchmark
                      )
                    | (ucn, benchmark) <- benchmarks
                    ]
            }

    pn = pkgName $ package $ packageDescription gpd

    libraryName :: LibraryName -> UnqualComponentName
    libraryName = fromMaybe (mkUnqualComponentName $ unPackageName pn) . libraryNameString

    libraries =
        concat
            [ [(LMainLibName, l) | l <- toList (condLibrary gpd)]
            , [(LSubLibName ucn, l) | (ucn, l) <- condSubLibraries gpd]
            ]
    tests =
        [ (ucn, fmap unvalidateTestSuite ts) | (ucn, ts) <- condTestSuites gpd
        ]

    benchmarks =
        [ (ucn, fmap unvalidateBenchmark b)
        | (ucn, b) <- condBenchmarks gpd
        ]

-- data Tree a where
--     Value :: a -> Tree a
--     Group :: [(String, Tree a)] -> Tree a
--     deriving (Show, Functor, Foldable, Traversable)

-- foldTree :: (a -> b) -> ([(String, b)] -> b) -> Tree a -> b
-- foldTree f _ (Value a) = f a
-- foldTree f g (Group as) = g [(n, foldTree f g a) | (n, a) <- as]

-- instance ToJSON a => ToJSON (Tree a) where
--     toJSON (Value a) = toJSON a
--     toJSON (Group as) = JsonObject [(an, toJSON a) | (an, a) <- as]

-- instance PrettyFieldClass a => PrettyFieldClass (Tree a) where
--     prettyField = foldTree prettyField (map (\(n, t) -> prettySection n [] t))
