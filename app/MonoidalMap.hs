{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}

module MonoidalMap where

import Data.Align
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Json (Json (..), ToJSON (..))

type FieldMap a = MonoidalMap String a

newtype MonoidalMap k v = MonoidalMap (Map k v)
    deriving (Show, Functor, Foldable, Traversable)

monoidalMap :: Ord k => [(k, v)] -> MonoidalMap k [v]
monoidalMap = MonoidalMap . Map.fromListWith (<>) . map (fmap (: []))

monoidalMap' :: (Ord k, Semigroup v) => [(k, v)] -> MonoidalMap k v
monoidalMap' = MonoidalMap . Map.fromListWith (<>)

singleton :: k -> v -> MonoidalMap k v
singleton k v = MonoidalMap (Map.singleton k v)

mapMaybe :: (a -> Maybe v) -> MonoidalMap k a -> MonoidalMap k v
mapMaybe f (MonoidalMap m) = MonoidalMap (Map.mapMaybe f m)

instance (Ord k, Semigroup v) => Semigroup (MonoidalMap k v) where
    MonoidalMap lhs <> MonoidalMap rhs = MonoidalMap $ Map.unionWith (<>) lhs rhs

instance (Ord k, Semigroup v) => Monoid (MonoidalMap k v) where
    mempty = MonoidalMap mempty

instance ToJSON v => ToJSON (MonoidalMap String v) where
    toJSON (MonoidalMap m) = JsonObject [(k, toJSON v) | (k, v) <- Map.toList m]

deriving via Map k instance Ord k => Semialign (MonoidalMap k)
deriving via Map k instance Ord k => Align (MonoidalMap k)
