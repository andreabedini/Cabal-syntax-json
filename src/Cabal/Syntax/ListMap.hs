{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | An association-list map that preserves insertion order.
--
-- Field and component order is significant in the JSON output, so throughout the
-- package we use this in place of a sorted @Data.Map@.
module Cabal.Syntax.ListMap
    ( ListMap
    , singleton
    , empty
    , fromList
    , fromListWith
    , Cabal.Syntax.ListMap.lookup
    , insert
    , union
    , unionWith
    ) where

import Data.Function (on)
import Data.List (partition)
import Data.List.NonEmpty qualified as NE

import Distribution.Utils.Generic (safeHead)

import Cabal.Syntax.Utils (FoldableWithIndex (..), Semialign (..), These (..), these)

-- | A map backed by a list of key/value pairs in insertion order.
newtype ListMap k v = ListMap [(k, v)]
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- | A map holding a single key/value pair.
singleton :: k -> v -> ListMap k v
singleton k v = ListMap [(k, v)]

-- | Insert a key/value pair. Any existing entry for the key is dropped and the new
-- one appended at the end (last-wins). See 'fromListWith' to merge instead.
insert :: Eq k => k -> a -> ListMap k a -> ListMap k a
insert k a (ListMap m) =
    ListMap (m' ++ [(k, a)])
  where
    (_, m') = pop k m

-- | The empty map.
empty :: ListMap k v
empty = ListMap mempty

-- | Look up the value stored for a key.
lookup :: Eq k => ListMap k v -> k -> Maybe v
lookup (ListMap m) k = fst (pop k m)

-- | Build a map from a list of pairs. Runs of /adjacent/ equal keys collapse to
-- their last value (use 'fromListWith' to merge non-adjacent duplicates).
fromList :: Eq k => [(k, v)] -> ListMap k v
fromList =
    ListMap
        . map NE.last
        . NE.groupBy ((==) `on` fst)

-- | Build a map from a list of pairs, combining the values of duplicate keys (even
-- non-adjacent ones) with the given function. Each key keeps the position of its
-- first occurrence.
fromListWith :: Eq k => (v -> v -> v) -> [(k, v)] -> ListMap k v
fromListWith f = foldl' (\acc (k, v) -> insertWith f k v acc) empty

-- | Insert a key/value, merging with any existing value for the key (existing
-- value on the left of @f@) and preserving the position of the key's first
-- occurrence. Appends if the key is absent.
insertWith :: Eq k => (v -> v -> v) -> k -> v -> ListMap k v -> ListMap k v
insertWith f k new (ListMap kvs) = ListMap (go kvs)
  where
    go [] = [(k, new)]
    go ((k', old) : rest)
        | k' == k = (k', f old new) : rest
        | otherwise = (k', old) : go rest

instance Eq k => Semialign (ListMap k) where
    align (ListMap lm) (ListMap rm) = ListMap $ go lm rm
      where
        go [] rs = [(rn, That rv) | (rn, rv) <- rs]
        go ((ln, lv) : ls) rs =
            case pop ln rs of
                (Nothing, rest) ->
                    (ln, This lv) : go ls rest
                (Just rv, rest) ->
                    (ln, These lv rv) : go ls rest

-- | Left-biased union: on a shared key the left map's value wins
-- (@'union' == 'unionWith' 'const'@).
union :: Eq k => ListMap k v -> ListMap k v -> ListMap k v
union = unionWith const

-- | Union of two maps, combining the values of shared keys with the given function.
unionWith :: Eq k => (v -> v -> v) -> ListMap k v -> ListMap k v -> ListMap k v
unionWith f l r = alignWith (these id id f) l r

instance (Semigroup v, Eq k) => Semigroup (ListMap k v) where
    (<>) = unionWith (<>)

instance (Semigroup v, Eq k) => Monoid (ListMap k v) where
    mempty = ListMap mempty

pop :: Eq a => a -> [(a, b)] -> (Maybe b, [(a, b)])
pop n l = (fmap snd (safeHead matches), rest)
  where
    (matches, rest) = partition ((n ==) . fst) l

instance FoldableWithIndex k (ListMap k) where
    itoList (ListMap kvs) = kvs
    ifoldMap f (ListMap kvs) = foldMap (uncurry f) kvs
