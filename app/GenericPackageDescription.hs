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
import Distribution.Fields (showFields)
import Distribution.Fields.Pretty (CommentPosition (..), PrettyField)
import Distribution.Pretty (Pretty (..), prettyShow)
import FieldMap (FieldMap, fromList)
import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..), JSONFieldGrammar, jsonFieldGrammar)
import Pretty (PrettyFieldClass (..), prettySection)
import Text.PrettyPrint (text)

-- import Pretty (PrettyFieldClass (..), prettySection)
-- import Text.PrettyPrint (text)

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
    pretty = text . showFields (const NoComment) . prettyField

-- | Transform a GenericPackageDescription into our representation.
-- This step already transform types associated with a field grammar into FieldMap (Fragment Json).
-- This is necessary to transform then into a single type.
runGenericPackageDescription
    :: CabalSpecVersion
    -> GenericPackageDescription
    -> GPD'
runGenericPackageDescription v gpd = GPD meta components
  where
    meta =
        mconcat
            [ jsonFieldGrammar v packageDescriptionFieldGrammar (packageDescription gpd)
            , FieldMap.fromList
                [ ( "custom-setup"
                  , ScalarFragment $ toJSON (jsonFieldGrammar v (setupBInfoFieldGrammar False) sbi)
                  )
                | sbi <- maybeToList (setupBuildInfo (packageDescription gpd))
                ]
            , FieldMap.fromList
                [ ( "source-repositories"
                  , ScalarFragment $ mkJsonObject v (prettyShow . repoKind) (sourceRepoFieldGrammar . repoKind) repos
                  )
                | let repos = sourceRepos (packageDescription gpd)
                , not (null repos)
                ]
            , FieldMap.fromList
                [ ( "flags"
                  , ScalarFragment $ mkJsonObject v (unFlagName . flagName) (flagFieldGrammar . flagName) flags
                  )
                | let flags = genPackageFlags gpd
                , not (null flags)
                ]
            ]

    components =
        Components
            { compLibraries = mkComponents v libraryName libraryFieldGrammar libraries
            , compForeignLibraries = mkComponents v id foreignLibFieldGrammar (condForeignLibs gpd)
            , compExecutables = mkComponents v id executableFieldGrammar (condExecutables gpd)
            , compTestSuites = mkComponents v id (const testSuiteFieldGrammar) tests
            , compBenchmarks = mkComponents v id (const benchmarkFieldGrammar) benchmarks
            }

    pn = pkgName $ package $ packageDescription gpd

    libraryName = fromMaybe (mkUnqualComponentName $ unPackageName pn) . libraryNameString

    libraries =
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

mkComponents
    :: Functor f
    => CabalSpecVersion
    -> (n -> UnqualComponentName)
    -> (n -> JSONFieldGrammar a a)
    -> [(n, f a)]
    -> Map UnqualComponentName (f (FieldMap (Fragment Json)))
mkComponents v mkName fg cs =
    Map.fromList
        [ (mkName ucn, fmap (jsonFieldGrammar v (fg ucn)) c)
        | (ucn, c) <- cs
        ]
