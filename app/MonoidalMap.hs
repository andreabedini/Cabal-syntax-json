{-# LANGUAGE DerivingVia #-}

module MonoidalMap
    ( FieldMap
    , singleton
    , toList
    ) where

import Data.Align
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Json (Json (..), ToJSON (..))

newtype FieldMap v = FieldMap (Map String v)
    deriving (Show, Functor, Foldable, Traversable)

singleton :: String -> v -> FieldMap v
singleton k v = FieldMap (Map.singleton k v)

toList :: FieldMap v -> [(String, v)]
toList (FieldMap m) = Map.toList m

instance Semigroup v => Semigroup (FieldMap v) where
    FieldMap lhs <> FieldMap rhs = FieldMap $ Map.unionWith (<>) lhs rhs

instance Semigroup v => Monoid (FieldMap v) where
    mempty = FieldMap mempty

instance ToJSON v => ToJSON (FieldMap v) where
    toJSON (FieldMap m) = JsonObject [(k, toJSON v) | (k, v) <- Map.toList m]

deriving via Map String instance Semialign FieldMap
deriving via Map String instance Align FieldMap
