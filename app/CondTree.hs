module CondTree where

import Data.Either (partitionEithers)
import Distribution.Compiler (CompilerId (..))
import Distribution.System (Arch, OS)
import Distribution.Types.CondTree (CondBranch (..), CondTree (..))
import Distribution.Types.Condition (Condition (..), simplifyCondition)
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Flag (FlagAssignment, lookupFlagAssignment)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Version (nullVersion)
import Distribution.Types.VersionRange (withinRange)

import Data.Foldable (Foldable (..))
import Data.Foldable1 (Foldable1 (..))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty qualified as NE
import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Package (Package (..))
import Distribution.PackageDescription
    ( LibraryName (..)
    , PackageDescription (..)
    , PackageFlag (..)
    , PackageIdentifier (..)
    , SetupBuildInfo
    , SourceRepo (..)
    , cAnd
    , cNot
    , libraryNameString
    , mapTreeData
    , unFlagName
    , unPackageName
    , unUnqualComponentName
    )
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
import Distribution.Utils.Json (Json (..), (.=))
import Json (ToJSON (..))
import JsonFieldGrammar
import MonoidalMap

jsonGenericPackageDescription :: GenericPackageDescription -> Json
jsonGenericPackageDescription gpd =
    JsonObject $
        concat
            [ map (fmap toJSON) $ jsonFieldGrammar v packageDescriptionFieldGrammar (packageDescription gpd)
            , [ "source-repos" .= toJSON (map (jsonSourceRepo v) repos)
              | let repos = sourceRepos (packageDescription gpd)
              , not (null repos)
              ]
            , [ "custom-setup" .= jsonCustomSetup v sbi
              | sbi <- toList (setupBuildInfo (packageDescription gpd))
              ]
            , [ "flags" .= JsonObject [unFlagName (flagName flag) .= jsonFlag v flag | flag <- flags]
              | let flags = genPackageFlags gpd
              , not (null flags)
              ]
            , [ "libraries"
                    .= JsonObject
                        [ (libraryName ln, toJSON (jsonCondTree v (libraryFieldGrammar ln) l))
                        | (ln, l) <- libraries
                        ]
              | not (null libraries)
              ]
            , [ "foreign-libraries"
                    .= JsonObject
                        [ prettyShow ucn .= toJSON (jsonCondTree v (foreignLibFieldGrammar ucn) c)
                        | (ucn, c) <- flibs
                        ]
              | let flibs = condForeignLibs gpd
              , not (null flibs)
              ]
            , [ "executables"
                    .= JsonObject
                        [ prettyShow ucn .= toJSON (jsonCondTree v (executableFieldGrammar ucn) c)
                        | (ucn, c) <- exes
                        ]
              | let exes = condExecutables gpd
              , not (null exes)
              ]
            , [ "test-suites"
                    .= JsonObject
                        [ prettyShow ucn .= toJSON (jsonCondTree v testSuiteFieldGrammar c)
                        | (ucn, c) <- testSuites
                        ]
              | not (null testSuites)
              ]
            , [ "benchmarks"
                    .= JsonObject
                        [ prettyShow ucn .= toJSON (jsonCondTree v benchmarkFieldGrammar c)
                        | (ucn, c) <- benchmarks
                        ]
              | not (null benchmarks)
              ]
            ]
  where
    v = specVersion $ packageDescription gpd
    pn = pkgName $ packageId $ packageDescription gpd
    libraryName = maybe (unPackageName pn) unUnqualComponentName . libraryNameString
    libraries =
        mconcat
            [ [(LMainLibName, l) | l <- toList (condLibrary gpd)]
            , [(LSubLibName ucn, l) | (ucn, l) <- condSubLibraries gpd]
            ]
    testSuites = map (fmap (mapTreeData unvalidateTestSuite)) $ condTestSuites gpd
    benchmarks = map (fmap (mapTreeData unvalidateBenchmark)) $ condBenchmarks gpd

jsonSourceRepo :: CabalSpecVersion -> SourceRepo -> Json
jsonSourceRepo v repo =
    JsonObject
        . map (fmap toJSON)
        $ jsonFieldGrammar v (sourceRepoFieldGrammar (repoKind repo)) repo

jsonCustomSetup :: CabalSpecVersion -> SetupBuildInfo -> Json
jsonCustomSetup v =
    JsonObject
        . map (fmap toJSON)
        . jsonFieldGrammar v (setupBInfoFieldGrammar False)

jsonFlag :: CabalSpecVersion -> PackageFlag -> Json
jsonFlag v flag =
    JsonObject
        . map (fmap toJSON)
        $ jsonFieldGrammar v (flagFieldGrammar (flagName flag)) flag

data Cond v a = Cond (Condition v) a
    deriving Show

instance (ToJSON a, ToJSON v) => ToJSON (Cond v a) where
    toJSON (Cond (Lit True) v) = toJSON v
    toJSON (Cond c v) = JsonObject ["_if" .= toJSON c, "_then" .= toJSON v]

jsonCondTree
    :: CabalSpecVersion
    -> JSONFieldGrammar s a
    -> CondTree v c s
    -> MonoidalMap String (Fragment (Cond v Json))
jsonCondTree v fg =
    monoidalMap'
        . foldCondTree (\c -> (fmap . fmap . fmap) (Cond c) . jsonFieldGrammar v fg)

foldCondTree
    :: Semigroup b
    => (Condition v -> a -> b)
    -> CondTree v c a
    -> b
foldCondTree f = go (Lit True)
  where
    go c (CondNode a _ []) =
        f c a
    go c (CondNode a _ (x : xs)) =
        f c a <> foldMap1 (goBranch c) (x NE.:| xs)
    goBranch c (CondBranch c' thenTree Nothing) =
        go (c `cAnd` c') thenTree
    goBranch c (CondBranch c' thenTree (Just elseTree)) =
        go (c `cAnd` c') thenTree <> go (c `cAnd` cNot c') elseTree

simplifyGPD :: (ConfVar -> Either ConfVar Bool) -> GenericPackageDescription -> GenericPackageDescription
simplifyGPD env = runIdentity . allCondTrees (Identity . simplifyCondTree env)

allCondTrees
    :: Applicative f
    => (forall a. Monoid a => CondTree ConfVar [Dependency] a -> f (CondTree ConfVar [Dependency] a))
    -> GenericPackageDescription
    -> f GenericPackageDescription
allCondTrees f (GenericPackageDescription p v a1 x1 x2 x3 x4 x5 x6) =
    GenericPackageDescription
        <$> pure p
        <*> pure v
        <*> pure a1
        <*> traverse f x1
        <*> (traverse . traverse) f x2
        <*> (traverse . traverse) f x3
        <*> (traverse . traverse) f x4
        <*> (traverse . traverse) f x5
        <*> (traverse . traverse) f x6

{- | Flattens a CondTree using a partial flag assignment. When a condition
cannot be evaluated, both branches are ignored.
-}
simplifyCondTree
    :: (Monoid a, Monoid c)
    => (v -> Either v Bool)
    -> CondTree v c a
    -> CondTree v c a
simplifyCondTree env (CondNode a c ifs) =
    CondNode a c branches <> mconcat trees
  where
    (trees, branches) = partitionEithers $ map simplifyCondBranch ifs
    simplifyCondBranch (CondBranch cv t me) =
        case simplifyCondition cv env of
            (Lit True, _) -> Left (simplifyCondTree env t)
            (Lit False, _) -> Left (maybe mempty (simplifyCondTree env) me)
            (cv', _) -> Right (CondBranch cv' t me)

mkEnv
    :: Maybe OS
    -> Maybe Arch
    -> Maybe CompilerId
    -> FlagAssignment
    -> ConfVar
    -> Either ConfVar Bool
mkEnv (Just os) _march _mcomp _flags (OS os') =
    Right $ os == os'
mkEnv _mos (Just arch) _mcomp _flags (Arch arch') =
    Right $ arch == arch'
mkEnv _mos _march (Just (CompilerId name ver)) _flags (Impl name' vr) =
    Right $ ver /= nullVersion && name == name' && ver `withinRange` vr
mkEnv _mos _march _mcomp flags (PackageFlag fn)
    | Just f <- lookupFlagAssignment fn flags =
        Right f
mkEnv _mos _march _mcomp _flags var = Left var
