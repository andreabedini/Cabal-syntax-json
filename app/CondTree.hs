module CondTree
    ( pushConditionals
    , Cond (..)
    , flattenCondTree
    , mkEnv
    , simplifyGPD
    , simplifyCondTree
    ) where

import Data.Either (partitionEithers)
import Data.Functor.Identity (Identity (..))

import Data.Semialign (Align (..), Semialign (..))
import Data.These (these)

import Distribution.Compiler (CompilerId (..))
import Distribution.PackageDescription (cNot)
import Distribution.System (Arch, OS)
import Distribution.Types.CondTree (CondBranch (..), CondTree (..), condIfThen, condIfThenElse)
import Distribution.Types.Condition (Condition (..), cAnd, simplifyCondition)
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Flag (FlagAssignment, lookupFlagAssignment)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Version (nullVersion)
import Distribution.Types.VersionRange (withinRange)
import Distribution.Utils.Json (Json (..), (.=))

import Data.Foldable1 (Foldable1 (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Json (ToJSON (..))

pushConditionals
    :: forall f v c a
     . (Align f, Semigroup a, Semigroup c)
    => CondTree v c (f a)
    -> f (CondTree v c a)
pushConditionals = go
  where
    go :: CondTree v c (f a) -> f (CondTree v c a)
    go (CondNode a d ifs) =
        case NE.nonEmpty ifs of
            Nothing ->
                fmap (\a' -> CondNode a' d []) a
            Just ne ->
                alignWith
                    ( these
                        (\a' -> CondNode a' d mempty)
                        (\ifs' -> reduce ifs')
                        (\a' ifs' -> CondNode a' d (NE.toList ifs'))
                    )
                    a
                    (crosswalk1 goBranch ne)

    goBranch :: CondBranch v c (f a) -> f (CondBranch v c a)
    goBranch (CondBranch c true mFalse) =
        case mFalse of
            Nothing ->
                fmap (condIfThen c) $ go true
            Just false ->
                alignWith
                    ( these
                        (condIfThen c)
                        (condIfThen (cNot c))
                        (condIfThenElse c)
                    )
                    (go true)
                    (go false)

reduce :: (Semigroup a, Semigroup c, Foldable1 f) => f (CondBranch v c a) -> CondTree v c a
reduce = foldMap1 $ \case
    (CondBranch cond (CondNode a c ifs) Nothing) ->
        CondNode a c (map (meetCondition cond) ifs)
    (CondBranch cond (CondNode a c ifs) (Just (CondNode a' c' ifs'))) ->
        CondNode a c (map (meetCondition cond) ifs)
            <> CondNode a' c' (map (meetCondition (cNot cond)) ifs')
  where
    meetCondition c (CondBranch c' t mf) = CondBranch (c `cAnd` c') t mf

flattenCondTree :: CondTree v c a -> Cond v a
flattenCondTree (CondNode a _ ifs) =
    Cond a (foldMap (goBranch (Lit True)) ifs)
  where
    go c (CondNode a' _ ifs') =
        (c, a') : foldMap (goBranch c) ifs'
    goBranch c (CondBranch c' thenTree Nothing) =
        go (c `cAnd` c') thenTree
    goBranch c (CondBranch c' thenTree (Just elseTree)) =
        go (c `cAnd` c') thenTree <> go (c `cAnd` cNot c') elseTree

data Cond v a = Cond a [(Condition v, a)]
    deriving (Show, Functor, Foldable, Traversable)

instance (ToJSON v, ToJSON a) => ToJSON (Cond v a) where
    toJSON (Cond a []) = toJSON a
    toJSON (Cond a as) = JsonArray (toJSON a : map jsonCond as)

-- instance ToJSON v => ToJSON (Cond v (Fragment Json)) where
--     toJSON (Cond (ScalarFragment a) []) = a
--     toJSON (Cond (ScalarFragment a) cs) = JsonArray (a : map jsonCond cs)
--     toJSON (Cond (ListLikeFragment as) []) = JsonArray (NE.toList as)
--     toJSON (Cond (ListLikeFragment as) cs) = JsonArray (NE.toList as <> map jsonCond cs)

jsonCond :: (ToJSON a, ToJSON b) => (a, b) -> Json
jsonCond (a, b) = JsonObject ["_if" .= toJSON a, "_then" .= toJSON b]

simplifyGPD
    :: (ConfVar -> Either ConfVar Bool) -> GenericPackageDescription -> GenericPackageDescription
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

-- | Simplifies a CondTree using a partial flag assignment. Conditions that cannot be evaluated are left untouched.
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
        case fst (simplifyCondition cv env) of
            (Lit True) -> Left (simplifyCondTree env t)
            (Lit False) -> Left (maybe mempty (simplifyCondTree env) me)
            -- TODO: this case does not recurse!
            cv' -> Right (CondBranch cv' t me)

mkEnv
    :: Maybe OS
    -> Maybe Arch
    -> Maybe CompilerId
    -> FlagAssignment
    -> ConfVar
    -> Either ConfVar Bool
mkEnv (Just os) _march _mcomp _flags (OS os') =
    Right (os == os')
mkEnv _mos (Just arch) _mcomp _flags (Arch arch') =
    Right (arch == arch')
mkEnv _mos _march (Just (CompilerId name ver)) _flags (Impl name' vr) =
    Right (ver /= nullVersion && name == name' && ver `withinRange` vr)
mkEnv _mos _march _mcomp flags (PackageFlag fn)
    | Just f <- lookupFlagAssignment fn flags =
        Right f
mkEnv _mos _march _mcomp _flags var = Left var

class (Functor t, Foldable1 t) => Crosswalk1 t where
    crosswalk1 :: Semialign f => (a -> f b) -> t a -> f (t b)
    crosswalk1 f = sequenceL1 . fmap f

    sequenceL1 :: Semialign f => t (f a) -> f (t a)
    sequenceL1 = crosswalk1 id

    {-# MINIMAL crosswalk1 | sequenceL1 #-}

instance Crosswalk1 NonEmpty where
    crosswalk1 :: Semialign f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
    crosswalk1 f = foldrMap1 (fmap NE.singleton . f) (alignWith cons . f)
      where
        cons = these NE.singleton id NE.cons
