{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Our package-description container, and the driver that builds it from Cabal's.
--
-- == Why not reuse Cabal's @GenericPackageDescription@?
--
-- Cabal's
-- 'Distribution.Types.GenericPackageDescription.GenericPackageDescription' is a fixed
-- record of strongly-typed fields (@packageDescription@, @condLibrary@,
-- @condExecutables@, …), each component a @CondTree ConfVar [Dependency]@ of a typed
-- @Library@\/@Executable@\/… value. Our aim is the opposite of strong typing: treat
-- every field uniformly as an opaque JSON 'Cabal.Syntax.JsonFieldGrammar.Fragment',
-- making /no/ assumptions about individual fields, and carry a shape that /changes
-- type/ as the pipeline rewrites it.
--
-- Hence 'GPD' is a generic, order-preserving, doubly-parameterised container:
--
-- @
-- data GPD a b = GPD ('FieldMap' a) ('ComponentMap' b)
-- @
--
--   * 'FieldMap' and 'ComponentMap' are insertion-order-preserving maps (over
--     'Cabal.Syntax.ListMap.ListMap'). Field and component order is significant in the
--     output, so a sorted @Map@ will not do.
--   * @a@ is the type of a top-level metadata field value and @b@ the per-component
--     representation. Both are parameters because the pipeline reuses the same
--     container at every stage: @b@ ranges from
--     'Cabal.Syntax.Pipeline.ComponentTreeCabal' down to
--     'Cabal.Syntax.Pipeline.JsonFields' as conditionals are restructured.
--
-- 'runGenericPackageDescription' is the bridge. It walks Cabal's
-- @GenericPackageDescription@ through Cabal's own 'Distribution.FieldGrammar'
-- machinery, but interpreted by our
-- 'Cabal.Syntax.JsonFieldGrammar.JSONFieldGrammar', so each typed field group is
-- rendered generically into a @FieldMap (Fragment Json)@ with no per-field code.
module Cabal.Syntax.GenericPackageDescription
    ( runGenericPackageDescription
    , CondTree'
    , GPD (..)
    , FieldMap (..)
    , ComponentMap (..)
    , ComponentName (..)
    , renderFields
    ) where

import Data.Foldable (Foldable (..))
import Data.Maybe (maybeToList)

import Distribution.CabalSpecVersion (CabalSpecVersion (..))
import Distribution.Fields.Pretty (CommentPosition (..), PrettyField (..), showFields)
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
import Distribution.Types.ComponentName (ComponentName (..))
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Flag (PackageFlag (..), unFlagName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.SourceRepo (SourceRepo (..))
import Distribution.Utils.Generic (toUTF8BS)
import Distribution.Utils.Json (Json (..))

import Text.PrettyPrint (Doc, text)

import Cabal.Syntax.Json (ToJSON (..))
import Cabal.Syntax.JsonFieldGrammar (Fragment (..), jsonFieldGrammar, jsonFieldGrammar')
import Cabal.Syntax.ListMap (ListMap)
import Cabal.Syntax.ListMap qualified as ListMap
import Cabal.Syntax.Pretty (PrettyFieldClass (..), prettySection)
import Cabal.Syntax.Utils (FoldableWithIndex (..), Semialign (..))

-- | Cabal's own 'CondTree' specialised to the variable and constraint types we use
-- (conditions over 'ConfVar', constraints aggregated as @[Dependency]@).
type CondTree' a = CondTree ConfVar [Dependency] a

-- | Our package representation: a 'FieldMap' of top-level metadata fields and a
-- 'ComponentMap' of components. The parameters @a@ (field value) and @b@ (component
-- representation) vary across pipeline stages; see the module header for the rationale.
data GPD a b = GPD (FieldMap a) (ComponentMap b)
    deriving (Show, Eq)

instance (ToJSON a, ToJSON b) => ToJSON (GPD a b) where
    toJSON (GPD a b) =
        JsonObject $
            [(k, toJSON v) | (k, v) <- itoList a]
            ++ [("components", toJSON b)]

instance (Pretty a, PrettyFieldClass b) => PrettyFieldClass (GPD a b) where
    prettyField (GPD a b) = prettyField a <> prettyField b

-- | Render a list of 'PrettyField's to a 'Doc' in cabal layout (without comments).
renderFields :: [PrettyField ann] -> Doc
renderFields = text . showFields (const NoComment)

-- | An order-preserving map from field name to value.
newtype FieldMap a = FieldMap (ListMap String a)
    deriving (Show, Eq, Functor)
    deriving Semigroup via ListMap String a
    deriving Semialign via ListMap String
    deriving (FoldableWithIndex String) via ListMap String

instance Pretty a => PrettyFieldClass (FieldMap a) where
    prettyField (FieldMap m) =
        [ PrettyField () (toUTF8BS n) (pretty a)
        | (n, a) <- itoList m
        ]

instance ToJSON a => ToJSON (FieldMap a) where
    toJSON (FieldMap m) =
        JsonObject [ (n, toJSON a) | (n, a) <- itoList m ]

-- | An order-preserving map from 'ComponentName' to a component's representation.
newtype ComponentMap a = ComponentMap (ListMap ComponentName a)
    deriving (Show, Eq, Functor)
    deriving (FoldableWithIndex ComponentName) via ListMap ComponentName

instance ToJSON a => ToJSON (ComponentMap a) where
    toJSON (ComponentMap m) =
        JsonObject [ (prettyShow n, toJSON a) | (n, a) <- itoList m ]

instance PrettyFieldClass a => PrettyFieldClass (ComponentMap a) where
    prettyField (ComponentMap m) =
        [ prettySection (prettyShow n) [] (prettyField a)
        | (n, a) <- itoList m
        ]

-- | Transform Cabal's 'GenericPackageDescription' into our 'GPD'.
--
-- This is the type-erasing step: every strongly-typed field group is run through its
-- field grammar with our 'Cabal.Syntax.JsonFieldGrammar.JSONFieldGrammar', collapsing
-- the disparate field types into the single uniform @FieldMap (Fragment Json)@ that the
-- rest of the pipeline operates on. See the module header for why we need our own
-- container at all.
runGenericPackageDescription
    :: CabalSpecVersion
    -> GenericPackageDescription
    -> GPD
        (Fragment Json)
        (CondTree ConfVar [Dependency] (FieldMap (Fragment Json)))
runGenericPackageDescription v gpd =
    GPD (FieldMap meta) (ComponentMap components)
  where
    meta :: ListMap String (Fragment Json)
    meta =
        ListMap.fromListWith (<>) $
            mconcat
                [ fmap (fmap (ScalarFragment . toJSON)) $
                    jsonFieldGrammar v packageDescriptionFieldGrammar (packageDescription gpd)
                , [ ( "custom-setup"
                    , ScalarFragment (jsonFieldGrammar' v (setupBInfoFieldGrammar False) sbi)
                    )
                  | sbi <- maybeToList (setupBuildInfo (packageDescription gpd))
                  ]
                , [ ("source-repositories", ScalarFragment (sourceRepos2json v repos))
                  | let repos = sourceRepos (packageDescription gpd)
                  , not (null repos)
                  ]
                , [ ("flags", ScalarFragment (flags2json v flags))
                  | let flags = genPackageFlags gpd
                  , not (null flags)
                  ]
                ]

    components =
        ListMap.fromList $
            mconcat
                [ [ (CLibName n, fmap (FieldMap . ListMap.fromListWith (<>) . jsonFieldGrammar v (libraryFieldGrammar n)) c)
                  | (n, c) <- libs
                  ]
                , [ (CFLibName n, fmap (FieldMap . ListMap.fromListWith (<>) . jsonFieldGrammar v (foreignLibFieldGrammar n)) c)
                  | (n, c) <- condForeignLibs gpd
                  ]
                , [ (CExeName n, fmap (FieldMap . ListMap.fromListWith (<>) . jsonFieldGrammar v (executableFieldGrammar n)) c)
                  | (n, c) <- condExecutables gpd
                  ]
                , [ (CTestName n, fmap (FieldMap . ListMap.fromListWith (<>) . jsonFieldGrammar v testSuiteFieldGrammar) c)
                  | (n, c) <- tests
                  ]
                , [ (CBenchName n, fmap (FieldMap . ListMap.fromListWith (<>) . jsonFieldGrammar v benchmarkFieldGrammar) c)
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
