{-# LANGUAGE DerivingVia #-}

module FieldMap
    ( FieldMap
    , singleton
    , toList
    , fromList
    , empty
    , ppFieldMap
    ) where

import Data.Align
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Json (ToJSON (..))
import Distribution.Utils.Json (Json(..))
import Distribution.Fields.Pretty (PrettyField)
import Distribution.Pretty (Pretty (..))
import Pretty (prettyField)

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

instance Semigroup v => Semigroup (FieldMap v) where
    FieldMap lhs <> FieldMap rhs = FieldMap $ Map.unionWith (<>) lhs rhs

instance Semigroup v => Monoid (FieldMap v) where
    mempty = FieldMap mempty

instance ToJSON v => ToJSON (FieldMap v) where
    toJSON (FieldMap m) = JsonObject [(k, toJSON v) | (k, v) <- Map.toList m]

deriving via Map String instance Semialign FieldMap
deriving via Map String instance Align FieldMap

ppFieldMap :: Pretty a => FieldMap a -> [PrettyField ()]
ppFieldMap it = [prettyField n (pretty a) | (n, a) <- FieldMap.toList it]
