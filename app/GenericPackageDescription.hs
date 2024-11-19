module GenericPackageDescription
    ( runGenericPackageDescription
    , Tree (..)
    , CondTree'
    , foldTree
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

import FieldMap (FieldMap)
import Json
import JsonFieldGrammar (Fragment (..), JSONFieldGrammar (..), jsonFieldGrammar)

data Tree a where
    Value :: a -> Tree a
    Group :: [(String, Tree a)] -> Tree a
    deriving (Show, Functor, Foldable, Traversable)

foldTree :: Monoid m => (a -> m) -> (String -> m -> m) -> Tree a -> m
foldTree f _ (Value a) = f a
foldTree f g (Group as) = foldMap (\(n, a) -> g n (foldTree f g a)) as

instance ToJSON a => ToJSON (Tree a) where
    toJSON (Value a) = toJSON a
    toJSON (Group as) = JsonObject [(an, toJSON a) | (an, a) <- as]

type CondTree' a = CondTree ConfVar [Dependency] a

-- | Transform a GenericPackageDescription into our representation.
-- This step already transform types associated with a field grammar into FieldMap (Fragment Json).
-- This is necessary to transform then into a single type.
runGenericPackageDescription
    :: CabalSpecVersion
    -> GenericPackageDescription
    -> ( FieldMap (Fragment Json)
       , [(String, Tree (FieldMap (Fragment Json)))]
       , [(String, Tree (CondTree' (FieldMap (Fragment Json))))]
       )
runGenericPackageDescription v gpd = (top, middle, bottom)
  where
    top = jsonFieldGrammar v packageDescriptionFieldGrammar (packageDescription gpd)

    middle =
        mconcat
            [ [ mkGroup
                    v
                    "source-repositories"
                    (prettyShow . repoKind)
                    (sourceRepoFieldGrammar . repoKind)
                    repos
              | let repos = sourceRepos (packageDescription gpd)
              , not (null repos)
              ]
            , [ ("custom-setup", Value $ jsonFgCustomSetup v sbi)
              | sbi <- maybeToList (setupBuildInfo (packageDescription gpd))
              ]
            , [ mkGroup v "flags" (unFlagName . flagName) (flagFieldGrammar . flagName) flags
              | let flags = genPackageFlags gpd
              , not (null flags)
              ]
            ]

    bottom =
        mconcat
            [ [ ("libraries", mkCondGroup v libraryName libraryFieldGrammar libraries)
              | not (null libraries)
              ]
            , [ ("foreign-libraries", mkCondGroup v unUnqualComponentName foreignLibFieldGrammar flibs)
              | let flibs = condForeignLibs gpd
              , not (null flibs)
              ]
            , [ ("executables", mkCondGroup v unUnqualComponentName executableFieldGrammar exes)
              | let exes = condExecutables gpd
              , not (null exes)
              ]
            , [ ("test-suites", mkCondGroup v unUnqualComponentName (const testSuiteFieldGrammar) tests)
              | not (null tests)
              ]
            , [ ("benchmarks", mkCondGroup v unUnqualComponentName (const benchmarkFieldGrammar) benchmarks)
              | not (null benchmarks)
              ]
            ]

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

mkGroup
    :: CabalSpecVersion
    -> String
    -> (s -> String)
    -> (s -> JSONFieldGrammar s a)
    -> [s]
    -> (String, Tree (FieldMap (Fragment Json)))
mkGroup v groupName mkName mkFg as =
    (groupName, elements)
  where
    elements = Group [(mkName a, Value $ jsonFieldGrammar v (mkFg a) a) | a <- as]

mkCondGroup
    :: CabalSpecVersion
    -> (n -> String)
    -> (n -> JSONFieldGrammar a s)
    -> [(n, CondTree' a)]
    -> Tree (CondTree' (FieldMap (Fragment Json)))
mkCondGroup v mkName mkFg comps =
    Group [(mkName cn, Value $ jsonFgCondTree v (mkFg cn) c) | (cn, c) <- comps]
