{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module GenericPackageDescription
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

import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..), jsonFieldGrammar, jsonFieldGrammar')
import ListMap (ListMap)
import ListMap qualified
import Pretty (PrettyFieldClass (..), prettySection)
import Utils (FoldableWithIndex (..), Semialign (..))

type CondTree' a = CondTree ConfVar [Dependency] a

data GPD a b = GPD (FieldMap a) (ComponentMap b)
    deriving (Show, Eq)

instance (ToJSON a, ToJSON b) => ToJSON (GPD a b) where
    toJSON (GPD a b) =
        JsonObject $
            [(k, toJSON v) | (k, v) <- itoList a]
            ++ [("components", toJSON b)]

instance (Pretty a, PrettyFieldClass b) => PrettyFieldClass (GPD a b) where
    prettyField (GPD a b) = prettyField a <> prettyField b

renderFields :: [PrettyField ann] -> Doc
renderFields = text . showFields (const NoComment)

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

-- | Transform a GenericPackageDescription into our representation.
-- This step already transform types associated with a field grammar into FieldMap (Fragment Json).
-- This is necessary to transform then into a single type.
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
        fmap ScalarFragment $
            ListMap.fromList $
                mconcat
                    [ fmap (fmap toJSON) $
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
            mconcat
                [ [ (CLibName n, fmap (FieldMap . ListMap.fromList . jsonFieldGrammar v (libraryFieldGrammar n)) c)
                  | (n, c) <- libs
                  ]
                , [ (CFLibName n, fmap (FieldMap . ListMap.fromList . jsonFieldGrammar v (foreignLibFieldGrammar n)) c)
                  | (n, c) <- condForeignLibs gpd
                  ]
                , [ (CExeName n, fmap (FieldMap . ListMap.fromList . jsonFieldGrammar v (executableFieldGrammar n)) c)
                  | (n, c) <- condExecutables gpd
                  ]
                , [ (CTestName n, fmap (FieldMap . ListMap.fromList . jsonFieldGrammar v testSuiteFieldGrammar) c)
                  | (n, c) <- tests
                  ]
                , [ (CBenchName n, fmap (FieldMap . ListMap.fromList . jsonFieldGrammar v benchmarkFieldGrammar) c)
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
