module CondTree where

import Data.Either (partitionEithers)
import Data.Functor.Identity (Identity (..))

import Data.Crosswalk (Crosswalk (..))
import Data.Semialign (Align, Semialign (..))
import Data.These (these)

import Distribution.Compiler (CompilerId (..))
import Distribution.PackageDescription (cNot)
import Distribution.System (Arch, OS)
import Distribution.Types.CondTree (CondBranch (..), CondTree (..))
import Distribution.Types.Condition (Condition (..), simplifyCondition)
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Flag (FlagAssignment, lookupFlagAssignment)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Version (nullVersion)
import Distribution.Types.VersionRange (withinRange)
import Distribution.Utils.Json (Json (..), (.=))

import Json (ToJSON (..))

test'V
    :: forall f v c a
     . (Align f, Monoid a)
    => CondTree v c (f a)
    -> f (CondTree v c a)
test'V = go
  where
    go :: CondTree v c (f a) -> f (CondTree v c a)
    go (CondNode a d ifs) =
        alignWith
            ( these
                (\a' -> CondNode a' d mempty)
                (\ifs' -> CondNode mempty d ifs')
                (\a' ifs' -> CondNode a' d ifs')
            )
            a
            (crosswalk goBranch ifs)

    goBranch :: CondBranch v c (f a) -> f (CondBranch v c a)
    goBranch (CondBranch c true Nothing) =
        fmap (\t -> CondBranch c t Nothing) $ go true
    goBranch (CondBranch c true (Just false)) =
        alignWith
            ( these
                (\t -> CondBranch c t Nothing)
                (\f -> CondBranch (cNot c) f Nothing)
                (\t f -> CondBranch c t (Just f))
            )
            (go true)
            (go false)

data Cond v a = Cond a [(Condition v, a)]
    deriving Show

instance (ToJSON a, ToJSON v) => ToJSON (Cond v a) where
    toJSON (Cond a ifs) =
        JsonArray $
            toJSON a
                : map
                    ( \(c, a') ->
                        JsonObject ["_if" .= toJSON c, "_then" .= toJSON a']
                    )
                    ifs

-- foldCondTree
--     :: Semigroup b
--     => (Condition v -> a -> b)
--     -> CondTree v c a
--     -> b
-- foldCondTree f = go (Lit True)
--   where
--     go c (CondNode a _ []) =
--         f c a
--     go c (CondNode a _ (x : xs)) =
--         f c a <> foldMap1 (goBranch c) (x NE.:| xs)
--     goBranch c (CondBranch c' thenTree Nothing) =
--         go (c `cAnd` c') thenTree
--     goBranch c (CondBranch c' thenTree (Just elseTree)) =
--         go (c `cAnd` c') thenTree <> go (c `cAnd` cNot c') elseTree

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
