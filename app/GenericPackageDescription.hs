module GenericPackageDescription
    ( runGenericPackageDescription
    , Grouped (..)
    ) where

import Data.Bifunctor (Bifunctor (..))
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
import Distribution.Types.CondTree (CondTree, mapTreeData)
import Distribution.Types.ConfVar (ConfVar)
import Distribution.Types.Flag (PackageFlag (..), unFlagName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.LibraryName (LibraryName (..), libraryNameString)
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.SetupBuildInfo (SetupBuildInfo)
import Distribution.Types.SourceRepo (SourceRepo (..))
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Json (Json (..), (.=))

import Distribution.Types.Dependency (Dependency)
import Distribution.Types.PackageName (unPackageName)
import Json (ToJSON (..))
import JsonFieldGrammar (Fragment, JSONFieldGrammar (..), jsonFieldGrammar)
import MonoidalMap (FieldMap)

-- | Transform a GenericPackageDescription into our representation.
-- This step already transform types associated with a field grammar into FieldMap (Fragment Json).
-- This is necessary to transform then into a single type.
runGenericPackageDescription
    :: CabalSpecVersion
    -> GenericPackageDescription
    -> ( FieldMap (Fragment Json)
       , [ Grouped
            (FieldMap (Fragment Json))
            (CondTree ConfVar [Dependency] (FieldMap (Fragment Json)))
         ]
       )
runGenericPackageDescription v gpd = (top, groups)
  where
    top = jsonFieldGrammar v packageDescriptionFieldGrammar (packageDescription gpd)
    groups =
        concat
            [ mkGroup
                "source-repositories"
                ( \sourceRepo ->
                    Entry
                        (prettyShow (repoKind sourceRepo))
                        (jsonFgSourceRepo v sourceRepo)
                )
                (sourceRepos (packageDescription gpd))
            , maybe
                mempty
                (\sbi -> [Entry "custom-setup" (jsonFgCustomSetup v sbi)])
                (setupBuildInfo (packageDescription gpd))
            , mkGroup
                "flags"
                (\flag -> Entry (unFlagName (flagName flag)) (jsonFgFlag v flag))
                (genPackageFlags gpd)
            , mkGroup
                "libraries"
                (mkEntryC v libraryFieldGrammar libraryName)
                $ concat
                    [ [(LMainLibName, l) | l <- toList (condLibrary gpd)]
                    , [(LSubLibName ucn, l) | (ucn, l) <- condSubLibraries gpd]
                    ]
            , mkGroup
                "foreign-libraries"
                (mkEntryC v foreignLibFieldGrammar unUnqualComponentName)
                (condForeignLibs gpd)
            , mkGroup
                "executables"
                (mkEntryC v executableFieldGrammar unUnqualComponentName)
                (condExecutables gpd)
            , mkGroup
                "test-suites"
                (mkEntryC v (const testSuiteFieldGrammar) unUnqualComponentName)
                [ (ucn, mapTreeData unvalidateTestSuite ts)
                | (ucn, ts) <- condTestSuites gpd
                ]
            , mkGroup
                "benchmarks"
                (mkEntryC v (const benchmarkFieldGrammar) unUnqualComponentName)
                [ (ucn, mapTreeData unvalidateBenchmark b)
                | (ucn, b) <- condBenchmarks gpd
                ]
            ]

    pn = pkgName $ package $ packageDescription gpd

    libraryName :: LibraryName -> String
    libraryName = maybe (unPackageName pn) unUnqualComponentName . libraryNameString

jsonFgSourceRepo :: CabalSpecVersion -> SourceRepo -> FieldMap (Fragment Json)
jsonFgSourceRepo v repo =
    jsonFieldGrammar v (sourceRepoFieldGrammar (repoKind repo)) repo

jsonFgCustomSetup :: CabalSpecVersion -> SetupBuildInfo -> FieldMap (Fragment Json)
jsonFgCustomSetup v sbi =
    jsonFieldGrammar v (setupBInfoFieldGrammar False) sbi

jsonFgFlag :: CabalSpecVersion -> PackageFlag -> FieldMap (Fragment Json)
jsonFgFlag v flag =
    jsonFieldGrammar v (flagFieldGrammar (flagName flag)) flag

jsonFgCondTree
    :: CabalSpecVersion
    -> JSONFieldGrammar a s
    -> CondTree v c a
    -> CondTree v c (FieldMap (Fragment Json))
jsonFgCondTree v fg = mapTreeData (jsonFieldGrammar v fg)

data Grouped a b where
    Entry
        :: String
        -> a
        -> Grouped a b
        -- ^ A named entry with no conditionals (e.g. source-repository, custom-setup)
    EntryC
        :: String
        -> b
        -> Grouped a b
        -- ^ A named entry with conditionals (e.g. library, executable, etc)
    Group
        :: String
        -> [Grouped a b]
        -> Grouped a b
        -- ^ A named group of entries. We use this to gather multiple named entries of the same
        -- type into groups. Typically this corresponds to sections in a cabal file (e.g. flags,
        -- libraries, etc).
    deriving (Show, Functor, Foldable, Traversable)

instance (ToJSON a, ToJSON b) => ToJSON (Grouped a b) where
    toJSON = JsonObject . go
    toJSONList = JsonObject . foldMap go

go :: (ToJSON b, ToJSON a) => Grouped a b -> [(String, Json)]
go (Entry name value) = [name .= toJSON value]
go (EntryC name value) = [name .= toJSON value]
go (Group name values) = [name .= JsonObject (foldMap go values)]

instance Bifunctor Grouped where
    bimap f _ (Entry name a) = Entry name (f a)
    bimap _ g (EntryC name b) = EntryC name (g b)
    bimap f g (Group name gs) = Group name (map (bimap f g) gs)

-- | Utility function to avoid creating empty groups.
mkGroup :: String -> (a -> Grouped b c) -> [a] -> [Grouped b c]
mkGroup name f things = [Group name (map f things) | not (null things)]

-- | Utility function to pave over some differences in how GenericPackageDescription is structured.
mkEntryC
    :: CabalSpecVersion
    -> (n -> JSONFieldGrammar b s)
    -> (n -> String)
    -> (n, CondTree v c b)
    -> Grouped a (CondTree v c (FieldMap (Fragment Json)))
mkEntryC v mkFg mkName (n, a) = EntryC (mkName n) (jsonFgCondTree v (mkFg n) a)
