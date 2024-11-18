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
import Distribution.Utils.Json (Json (..), (.=))

import Data.Maybe (maybeToList)
import FieldMap (FieldMap, fromList)
import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..), JSONFieldGrammar (..), jsonFieldGrammar, scalarFragment)

data Grouped a where
    Entry
        :: a
        -> Grouped a
    Group
        :: [(String, Grouped a)]
        -> Grouped a
        -- ^ A named group of entries. We use this to gather multiple named entries of the same
        -- type into groups. Typically this corresponds to sections in a cabal file (e.g. flags,
        -- libraries, etc).
    deriving (Show, Functor, Foldable, Traversable)

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
    [ Entry $ mkCondNode $ jsonFieldGrammar v packageDescriptionFieldGrammar (packageDescription gpd)
    , fmap mkCondNode $
        mkGroup
            v
            "source-repositories"
            prettyShow
            sourceRepoFieldGrammar
            [ (repoKind sourceRepo, sourceRepo)
            | sourceRepo <- sourceRepos (packageDescription gpd)
            ]
    , Group
        [ ("custom-setup", Entry $ mkCondNode $ jsonFgCustomSetup v sbi)
        | sbi <- maybeToList (setupBuildInfo (packageDescription gpd))
        ]
    , fmap mkCondNode $
        mkGroup
            v
            "flags"
            unFlagName
            flagFieldGrammar
            [(flagName flag, flag) | flag <- genPackageFlags gpd]
    , mkCondTreeGroup v "libraries" libraryName libraryFieldGrammar libraries
    , mkCondTreeGroup
        v
        "foreign-libraries"
        unUnqualComponentName
        foreignLibFieldGrammar
        (condForeignLibs gpd)
    , mkCondTreeGroup v "executables" unUnqualComponentName executableFieldGrammar (condExecutables gpd)
    , mkCondTreeGroup v "test-suites" unUnqualComponentName (const testSuiteFieldGrammar) tests
    , mkCondTreeGroup v "benchmarks" unUnqualComponentName (const benchmarkFieldGrammar) benchmarks
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

-- instance ToJSON a => ToJSON (Grouped a) where
-- toJSON = JsonObject . go
-- toJSONList = JsonObject . foldMap go

-- go :: ToJSON a => Grouped a -> [(String, Json)]
-- go (Entry name value) = [name .= toJSON value]
-- go (EntryC name value) = [name .= toJSON value]
-- go (Group name values) = [name .= JsonObject (foldMap go values)]

-- instance Bifunctor Grouped where
--     -- bimap f _ (Entry name a) = Entry name (f a)
--     bimap _ g (EntryC name b) = EntryC name (g b)
--     bimap f g (Group name gs) = Group name (map (bimap f g) gs)

-- mkGroup
--     :: CabalSpecVersion
--     -> String
--     -> (t -> String)
--     -> (CabalSpecVersion -> t -> JSONFieldGrammar a s)
--     -> [(t, a)]
--     -> [(String, Grouped a)]
mkGroup
    :: CabalSpecVersion
    -> String
    -> (n -> String)
    -> (n -> JSONFieldGrammar a s)
    -> [(n, a)]
    -> Grouped (FieldMap (Fragment Json))
mkGroup v groupName mkName mkFg things =
    Group
        [ ( groupName
          , Group
                [ (mkName n, Entry $ jsonFieldGrammar v (mkFg n) c)
                | (n, c) <- things
                ]
          )
        | not (null things)
        ]

-- | Utility function to pave over some differences in how GenericPackageDescription is structured.
mkCondTreeGroup
    :: CabalSpecVersion
    -> String
    -> (t -> String)
    -> (t -> JSONFieldGrammar a s)
    -> [(t, CondTree v c a)]
    -> Grouped (CondTree v c (FieldMap (Fragment Json)))
mkCondTreeGroup v groupName mkName mkFg things =
    Group
        [ ( groupName
          , Group
                [ (mkName n, Entry $ jsonFgCondTree v (mkFg n) c)
                | (n, c) <- things
                ]
          )
        | not (null things)
        ]

-- mkEntryC
--     :: CabalSpecVersion
--     -> (n -> JSONFieldGrammar b s)
--     -> (n -> String)
--     -> (n, CondTree v c b)
--     -> Grouped (CondTree v c (FieldMap (Fragment Json)))
-- mkEntryC v mkFg mkName (n, a) = EntryC (mkName n) (jsonFgCondTree v (mkFg n) a)

-- [ [ ( "libraries"
--     , Group
--         [ (libraryName ln, Entry $ jsonFgCondTree v (libraryFieldGrammar ln) l)
--         | (ln, l) <- libraries
--         ]
--     )
--   | let libraries =
--             mconcat
--                 [ [(LMainLibName, l) | l <- toList (condLibrary gpd)]
--                 , [(LSubLibName ucn, l) | (ucn, l) <- condSubLibraries gpd]
--                 ]
--   , not (null libraries)
--   ]
-- ]
