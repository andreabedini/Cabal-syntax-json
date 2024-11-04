module CondTree (mkEnv, simplifyGPD) where

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

import Data.Functor.Identity

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
