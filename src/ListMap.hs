{-# LANGUAGE DerivingVia #-}

module ListMap
    ( ListMap
    , singleton
    , toList
    , fromList
    , empty
    -- , unionWith
    -- , foldMapWithKey
    , These (..)
    , ListMap.lookup
    , align
    , alignWith
    -- , fromListWith
    , union
    -- , insert
    ) where

import Data.Function (on)
import Data.List (partition)
import Data.List.NonEmpty qualified as NE

import Distribution.Fields.Pretty (CommentPosition (..), PrettyField (..), showFields)
import Distribution.Pretty (Pretty (..))
import Distribution.Utils.Generic (safeHead, toUTF8BS)
import Distribution.Utils.Json (Json (..))

import Text.PrettyPrint (text)

import Json (ToJSON (..))
import Pretty (PrettyFieldClass (..))
import These (Semialign (..), These (..), these)

newtype ListMap k v = ListMap [(k, v)]
    deriving (Eq, Show, Functor, Foldable, Traversable)

singleton :: k -> v -> ListMap k v
singleton k v = ListMap [(k, v)]

-- insert :: Eq k => k -> a -> ListMap k a -> ListMap k a
-- insert k a (ListMap m) =
--     ListMap (m' ++ [(k, a)])
--   where
--     (_, m') = pop k m

empty :: ListMap k v
empty = ListMap mempty

lookup :: Eq k => ListMap k v -> k -> Maybe v
lookup (ListMap m) k = fst (pop k m)

toList :: ListMap k v -> [(k, v)]
toList (ListMap m) = m

fromList :: Eq k => [(k, v)] -> ListMap k v
fromList =
    ListMap
        . map NE.last
        . NE.groupBy ((==) `on` fst)

-- fromListWith :: (Semigroup v, Eq k) => (v -> v -> v) -> [(k, v)] -> ListMap k v
-- fromListWith f =
--     ListMap
--         . map (\ne -> (fst (NE.head ne), foldl1' f (NE.map snd ne)))
--         . NE.groupBy ((==) `on` fst)

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

-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@.
-- It prefers @t1@ when duplicate keys are encountered, -- i.e. (@'union' == 'unionWith' 'const'@).
union :: Eq k => ListMap k v -> ListMap k v -> ListMap k v
union = unionWith const

unionWith :: Eq k => (v -> v -> v) -> ListMap k v -> ListMap k v -> ListMap k v
unionWith f l r = alignWith (these id id f) l r

-- foldMapWithKey :: Monoid m => (k -> a -> m) -> ListMap k a -> m
-- foldMapWithKey f (ListMap lm) = foldMap (uncurry f) lm

instance (Semigroup v, Eq k) => Semigroup (ListMap k v) where
    (<>) = unionWith (<>)

instance (Semigroup v, Eq k) => Monoid (ListMap k v) where
    mempty = ListMap mempty

instance ToJSON v => ToJSON (ListMap String v) where
    toJSON (ListMap m) = JsonObject [(k, toJSON v) | (k, v) <- m]

instance Pretty a => PrettyFieldClass (ListMap String a) where
    prettyField (ListMap m) =
        [ PrettyField () (toUTF8BS n) (pretty a) | (n, a) <- m
        ]

instance Pretty a => Pretty (ListMap String a) where
    pretty = text . showFields (const NoComment) . prettyField

pop :: Eq a => a -> [(a, b)] -> (Maybe b, [(a, b)])
pop n l = (fmap snd (safeHead matches), rest)
  where
    (matches, rest) = partition ((n ==) . fst) l
