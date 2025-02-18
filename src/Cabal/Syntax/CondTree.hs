{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Operations on the conditional structure of Cabal files.
module Cabal.Syntax.CondTree
    ( -- ** Representation
      CondTree (..)
    , CondNode (..)
    , convertCondTree

      -- ** Transformations
    , pushConditionals
    , simplifyCondTree
    , Guarded (..)
    , defragment
    , flattenCondTree
    ) where

import Data.Foldable1 (Foldable1 (..))
import Data.Functor.Compose (Compose (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE

import Distribution.PackageDescription (cNot)
import Distribution.Types.CondTree qualified as C (CondBranch (..), CondTree (..))
import Distribution.Types.Condition (Condition (..), cAnd, simplifyCondition)
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Utils.Json (Json (..), (.=))

import Cabal.Syntax.GenericPackageDescription (FieldMap)
import Cabal.Syntax.Json (ToJSON (..))
import Cabal.Syntax.JsonFieldGrammar (Fragment (..))
import Cabal.Syntax.Pretty (PrettyFieldClass (..), ppCondition, prettySection)
import Cabal.Syntax.Utils (Semialign (..), These (..))

-- | An alternative representation of the conditional structure of a Cabal file:
-- a non-empty list of nodes.
newtype CondTree v a = CondTree (NonEmpty (CondNode v a))
    deriving (Show, Functor, Foldable, Traversable, Semigroup)
    deriving Foldable1 via Compose NonEmpty (CondNode v)

-- | A 'CondNode' either carries a value of type 'a' or a conditional
-- statement on the variables 'v', a 'CondTree' to follow if the condition is
-- true and, optionally, a 'CondTree' to follow if the condition is false.
data CondNode v a
    = CondNode a
    | CondIfThen (Condition v) (CondTree v a)
    | CondIfThenElse (Condition v) (CondTree v a) (CondTree v a)
    deriving (Show, Functor, Foldable, Traversable)

instance Foldable1 (CondNode v) where
    foldMap1 f (CondNode a) = f a
    foldMap1 f (CondIfThen _ t) = foldMap1 f t
    foldMap1 f (CondIfThenElse _ t e) = foldMap1 f t <> foldMap1 f e

instance PrettyFieldClass a => PrettyFieldClass (CondTree ConfVar a) where
    prettyField (CondTree nodes) =
        foldMap1 prettyField nodes

instance PrettyFieldClass a => PrettyFieldClass (CondNode ConfVar a) where
    prettyField (CondNode a) = prettyField a
    prettyField (CondIfThen c t) =
        [ prettySection "if" [ppCondition c] $
            [ prettySection "then" [] (foldMap prettyField t)
            ]
        ]
    prettyField (CondIfThenElse c thenTree elseTree) =
        [ prettySection "if" [ppCondition c] $
            [ prettySection "then" [] (foldMap prettyField thenTree)
            , prettySection "else" [] (foldMap prettyField elseTree)
            ]
        ]

-- | This the main transformation of the conditional structure.
-- We want to obtain a structure where the fields of (e.g.) 'Distribution.Types.BuildInfo'
-- guarded by conditions rather than 'Distribution.Types.BuildInfo' itself.
pushConditionals
    :: forall v a
     . Semigroup a
    => CondTree v (FieldMap a)
    -> FieldMap (CondTree v a)
pushConditionals (CondTree nodes) = foldMap1 go nodes
  where
    go :: CondNode v (FieldMap a) -> FieldMap (CondTree v a)
    go (CondNode a) =
        fmap
            (CondTree . NE.singleton . CondNode)
            a
    go (CondIfThen c t) =
        fmap
            (CondTree . NE.singleton . (\x -> CondIfThen c x))
            (pushConditionals t)
    go (CondIfThenElse c t e) =
        alignWith
            ( \case
                This t' ->
                    CondTree (NE.singleton (CondIfThen c t'))
                That e' ->
                    CondTree (NE.singleton (CondIfThen (cNot c) e'))
                These t' e' ->
                    CondTree (NE.singleton (CondIfThenElse c t' e'))
            )
            (pushConditionals t)
            (pushConditionals e)

-- | Simplifies a CondTree using a partial flag assignment. Conditions that
-- cannot be evaluated are left untouched.
simplifyCondTree
    :: forall v a
     . Monoid a
    => (v -> Either v Bool)
    -> CondTree v a
    -> Maybe (CondTree v a)
simplifyCondTree eval (CondTree nodes) =
    CondTree <$> NE.nonEmpty (foldMap go nodes)
  where
    go :: CondNode v a -> [CondNode v a]
    go (CondNode a) =
        [CondNode a]
    go (CondIfThen c (CondTree ts)) =
        case fst (simplifyCondition c eval) of
            Lit True ->
                NE.toList ts
            Lit False -> []
            c' -> case NE.nonEmpty (foldMap go ts) of
                Nothing -> []
                Just ts' -> [CondIfThen c' (CondTree ts')]
    go (CondIfThenElse c (CondTree ts) (CondTree es)) =
        case fst (simplifyCondition c eval) of
            Lit True ->
                NE.toList ts
            Lit False ->
                NE.toList es
            c' -> case NE.nonEmpty (foldMap go ts <> foldMap go es) of
                Nothing -> []
                Just ts' -> [CondIfThen c' (CondTree ts')]

-- | Convert 'Distribution.Types.CondTree.CondTree' from Cabal-syntax into our 'CondTree'.
convertCondTree :: C.CondTree v c a -> CondTree v a
convertCondTree (C.CondNode a _ ifs) =
    CondTree (CondNode a :| map convertCondBranch ifs)

convertCondBranch :: C.CondBranch v c a -> CondNode v a
convertCondBranch (C.CondBranch c thenTree Nothing) =
    CondIfThen
        c
        (convertCondTree thenTree)
convertCondBranch (C.CondBranch c thenTree (Just elseTree)) =
    CondIfThenElse
        c
        (convertCondTree thenTree)
        (convertCondTree elseTree)

-- | A value 'a' guarded by a 'Condition' on variables 'v'.
data Guarded v a = Guarded (Condition v) a
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Transform a tree of conditions into a flat list of values guarded by their
-- cumulative condition.
flattenCondTree
    :: forall v a
     . Semigroup a
    => CondTree v a
    -> NonEmpty (Guarded v a)
flattenCondTree (CondTree nodes) =
    foldMap1 (go (Lit True)) nodes
  where
    go c (CondNode a) =
        NE.singleton (Guarded c a)
    go c (CondIfThen c' (CondTree ts)) =
        foldMap1 (go (c `cAnd` c')) ts
    go c (CondIfThenElse c' (CondTree ts) (CondTree es)) =
        foldMap1 (go (c `cAnd` c')) ts <> foldMap1 (go (c `cAnd` cNot c')) es

defragment :: ToJSON v => NonEmpty (Guarded v (Fragment Json)) -> Fragment Json
defragment =
    foldMap1 $ \case
        Guarded (Lit True) a -> a
        Guarded c a ->
            ListLikeFragment $
                NE.singleton $
                    JsonObject
                        [ "_if" .= toJSON c
                        , "_then" .= toJSON a
                        ]
