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

import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import Data.Functor.Identity
import Debug.Trace (traceShowId)
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
            [ map (second toJSON) $ jsonFieldGrammar v packageDescriptionFieldGrammar (packageDescription gpd)
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
        . map (second toJSON)
        $ jsonFieldGrammar v (sourceRepoFieldGrammar (repoKind repo)) repo

jsonCustomSetup :: CabalSpecVersion -> SetupBuildInfo -> Json
jsonCustomSetup v =
    JsonObject
        . map (second toJSON)
        . jsonFieldGrammar v (setupBInfoFieldGrammar False)

jsonFlag :: CabalSpecVersion -> PackageFlag -> Json
jsonFlag v flag =
    JsonObject
        . map (second toJSON)
        $ jsonFieldGrammar v (flagFieldGrammar (flagName flag)) flag

type FieldMap a = MonoidalMap String a

data Cond a = Cond (Condition ConfVar) a
    deriving Show

instance ToJSON a => ToJSON (Cond a) where
    toJSON (Cond (Lit True) v) = toJSON v
    toJSON (Cond c v) = JsonObject ["_if" .= toJSON c, "_then" .= toJSON v]

-- jsonCondTree
--     :: CabalSpecVersion
--     -> JSONFieldGrammar a a
--     -> CondTree ConfVar c a
--     -> FieldMap [Fragment CondJson]
-- jsonCondTree v fg = go (Lit True) . mapTreeData (monoidalMap . jsonFieldGrammar v fg)
-- where
--   go :: Condition ConfVar -> CondTree ConfVar c (FieldMap JsonFragment) -> FieldMap [Fragment CondJson]
--   go c (CondNode it _ ifs) =
--       fmap (traverse (pure . CondJson c)) it <> foldMap (goBranch c) ifs

--   goBranch c (CondBranch c' thenTree elseTree) =
--       go (c `cAnd` c') thenTree <> foldMap (go (c `cAnd` cNot c')) elseTree
jsonCondTree
    :: CabalSpecVersion
    -> JSONFieldGrammar s s
    -> CondTree ConfVar c s
    -> FieldMap [Fragment (Cond Json)]
jsonCondTree v fg = traceShowId . x v fg

foldCondTree
    :: Monoid b
    => (Condition v -> a -> b)
    -> CondTree v c a
    -> b
foldCondTree f = go (Lit True)
  where
    go c (CondNode a _ ifs) =
        f c a <> foldMap (goBranch c) ifs
    goBranch c (CondBranch c' thenTree elseTree) =
        go (c `cAnd` c') thenTree <> foldMap (go (c `cAnd` cNot c')) elseTree

x :: CabalSpecVersion -> JSONFieldGrammar s s -> CondTree ConfVar c s -> FieldMap [Fragment (Cond Json)]
x v fg =
    foldCondTree $ \c ->
        (fmap . fmap . fmap) (Cond c)
            . monoidalMap
            . jsonFieldGrammar v fg

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
