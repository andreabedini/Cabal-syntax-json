{-# LANGUAGE CPP #-}

module MonoidalMap where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Json (Json (..), ToJSON (..))

newtype MonoidalMap k v = MonoidalMap (Map k v)
    deriving (Show, Functor, Foldable, Traversable)

monoidalMap :: Ord k => [(k, v)] -> MonoidalMap k [v]
monoidalMap = MonoidalMap . Map.fromListWith (<>) . map (fmap (: []))

instance (Ord k, Monoid v) => Semigroup (MonoidalMap k v) where
    MonoidalMap lhs <> MonoidalMap rhs = MonoidalMap $ Map.unionWith (<>) lhs rhs

instance (Ord k, Monoid v) => Monoid (MonoidalMap k v) where
    mempty = MonoidalMap mempty

instance ToJSON v => ToJSON (MonoidalMap String v) where
    toJSON (MonoidalMap m) = JsonObject [(k, toJSON v) | (k, v) <- Map.toList m]
