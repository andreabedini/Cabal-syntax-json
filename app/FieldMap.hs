{-# LANGUAGE DerivingVia #-}

module FieldMap
    ( FieldMap
    , singleton
    , toList
    , fromList
    , empty
    , unionWith
    , foldMapWithKey
    ) where

import Distribution.Fields.Pretty (CommentPosition (..), PrettyField (..), showFields)
import Distribution.Pretty (Pretty (..))
import Distribution.Utils.Generic (toUTF8BS)
import Distribution.Utils.Json (Json (..))

import Data.Align
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Text.PrettyPrint (text)

import Json (ToJSON (..))
import Pretty (PrettyFieldClass (..))

newtype FieldMap v = FieldMap (Map String v)
    deriving (Show, Functor, Foldable, Traversable)

singleton :: String -> v -> FieldMap v
singleton k v = FieldMap (Map.singleton k v)

toList :: FieldMap v -> [(String, v)]
toList (FieldMap m) = Map.toList m

fromList :: Semigroup v => [(String, v)] -> FieldMap v
fromList = FieldMap . Map.fromListWith (<>)

empty :: FieldMap v
empty = FieldMap mempty

unionWith :: (v -> v -> v) -> FieldMap v -> FieldMap v -> FieldMap v
unionWith f (FieldMap lm) (FieldMap rm) = FieldMap (Map.unionWith f lm rm)

foldMapWithKey :: Monoid m => (String -> a -> m) -> FieldMap a -> m
foldMapWithKey f (FieldMap lm) = Map.foldMapWithKey f lm

instance Semigroup v => Semigroup (FieldMap v) where
    FieldMap lhs <> FieldMap rhs = FieldMap $ Map.unionWith (<>) lhs rhs

instance Semigroup v => Monoid (FieldMap v) where
    mempty = FieldMap mempty

instance ToJSON v => ToJSON (FieldMap v) where
    toJSON (FieldMap m) = JsonObject [(k, toJSON v) | (k, v) <- Map.toList m]

instance Pretty a => PrettyFieldClass (FieldMap a) where
    prettyField it = [PrettyField () (toUTF8BS n) (pretty a) | (n, a) <- FieldMap.toList it]

instance Pretty a => Pretty (FieldMap a) where
    pretty = text . showFields (const NoComment) . prettyField

deriving via Map String instance Semialign FieldMap
deriving via Map String instance Align FieldMap
