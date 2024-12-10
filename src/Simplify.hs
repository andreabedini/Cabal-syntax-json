{-# LANGUAGE RecordWildCards #-}

module Simplify where

import Distribution.Compat.Prelude (isJust, partitionEithers)
import Distribution.Compiler (CompilerId (..))
import Distribution.System (Arch, OS)
import Distribution.Types.CondTree (CondBranch (..), CondTree (..))
import Distribution.Types.Condition (Condition (..), simplifyCondition)
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Flag (FlagAssignment, PackageFlag (..), lookupFlagAssignment)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Version (nullVersion)
import Distribution.Types.VersionRange (withinRange)

simplifyGPD
    :: Env
    -> GenericPackageDescription
    -> GenericPackageDescription
simplifyGPD env (GenericPackageDescription{..}) =
    GenericPackageDescription
        packageDescription
        gpdScannedVersion
        (filterFlags env genPackageFlags)
        (fmap (simplifyCondTree eval) condLibrary)
        ((fmap . fmap) (simplifyCondTree eval) condSubLibraries)
        ((fmap . fmap) (simplifyCondTree eval) condForeignLibs)
        ((fmap . fmap) (simplifyCondTree eval) condExecutables)
        ((fmap . fmap) (simplifyCondTree eval) condTestSuites)
        ((fmap . fmap) (simplifyCondTree eval) condBenchmarks)
  where
    eval :: ConfVar -> Either ConfVar Bool
    eval = applyEnv env

-- | Simplifies a CondTree using a partial flag assignment. Conditions that cannot be evaluated are left untouched.
simplifyCondTree
    :: forall v c a
     . (Monoid a, Monoid c)
    => (v -> Either v Bool)
    -> CondTree v c a
    -> CondTree v c a
simplifyCondTree eval (CondNode a c ifs) =
    CondNode a c branches <> mconcat trees
  where
    (trees, branches) = partitionEithers $ map (simplifyCondBranch eval) ifs

-- | Simplify a CondBranch using a partial variable assignment. Conditions that cannot be evaluated
-- are left unchanged. When we simplify a CondBranch the condition might become always true,
-- transforming the CondBranch into a CondTree. Therefore this function returns either a CondTree or
-- a CondBranch.
simplifyCondBranch
    :: (Monoid a, Monoid c)
    => (v -> Either v Bool)
    -> CondBranch v c a
    -> Either (CondTree v c a) (CondBranch v c a)
simplifyCondBranch eval (CondBranch cv t me) =
    case fst (simplifyCondition cv eval) of
        Lit True -> Left $ simplifyCondTree eval t
        Lit False -> Left $ maybe mempty (simplifyCondTree eval) me
        cv' -> Right $ CondBranch cv' (simplifyCondTree eval t) (fmap (simplifyCondTree eval) me)

-- | Filter out the flags defined in the environment
filterFlags :: Env -> [PackageFlag] -> [PackageFlag]
filterFlags Env{envFlags} = filter (\f -> isJust $ lookupFlagAssignment (flagName f) envFlags)

data Env = Env
    { envOS :: Maybe OS
    , envArch :: Maybe Arch
    , envCompiler :: Maybe CompilerId
    , envFlags :: FlagAssignment
    }

applyEnv
    :: Env
    -> ConfVar
    -> Either ConfVar Bool
applyEnv Env{envOS = Just os} (OS os') =
    Right (os == os')
applyEnv Env{envArch = Just arch} (Arch arch') =
    Right (arch == arch')
applyEnv Env{envCompiler = Just (CompilerId comp ver)} (Impl comp' ver') =
    Right (ver /= nullVersion && ver `withinRange` ver' && comp == comp')
applyEnv Env{envFlags} (PackageFlag fn) =
    case lookupFlagAssignment fn envFlags of
        Nothing -> Left (PackageFlag fn)
        Just b -> Right b
applyEnv _ var = Left var
