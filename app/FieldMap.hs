{-# LANGUAGE DerivingVia #-}

module FieldMap
    ( FieldMap
    , singleton
    , toList
    , fromList
    , empty
    -- , unionWith
    -- , foldMapWithKey
    , These (..)
    , FieldMap.lookup
    , align
    , alignWith
    -- , fromListWith
    , union
    -- , insert
    ) where

import Data.Foldable1 (foldl1')
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

newtype FieldMap v = FieldMap [(String, v)]
    deriving (Eq, Show, Functor, Foldable, Traversable)

singleton :: String -> v -> FieldMap v
singleton k v = FieldMap [(k, v)]

insert :: String -> a -> FieldMap a -> FieldMap a
insert k a (FieldMap m) = let (_, m') = pop k m in FieldMap (m' ++ [(k, a)])

empty :: FieldMap v
empty = FieldMap mempty

lookup :: FieldMap v -> String -> Maybe v
lookup (FieldMap m) n = Prelude.lookup n m

toList :: FieldMap v -> [(String, v)]
toList (FieldMap m) = m

fromList :: [(String, v)] -> FieldMap v
fromList =
    FieldMap
        . map NE.last
        . NE.groupBy ((==) `on` fst)

fromListWith :: Semigroup v => (v -> v -> v) -> [(String, v)] -> FieldMap v
fromListWith f =
    FieldMap
        . map (\ne -> (fst (NE.head ne), foldl1' f (NE.map snd ne)))
        . NE.groupBy ((==) `on` fst)

instance Semialign FieldMap where
    align :: FieldMap b -> FieldMap c -> FieldMap (These b c)
    align (FieldMap lm) (FieldMap rm) = FieldMap $ go lm rm
      where
        go :: Eq a => [(a, b)] -> [(a, c)] -> [(a, These b c)]
        go [] rs = [(rn, That rv) | (rn, rv) <- rs]
        go ((ln, lv) : ls) rs =
            case pop ln rs of
                (Nothing, rest) ->
                    (ln, This lv) : go ls rest
                (Just rv, rest) ->
                    (ln, These lv rv) : go ls rest

-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@.
-- It prefers @t1@ when duplicate keys are encountered, -- i.e. (@'union' == 'unionWith' 'const'@).
union :: FieldMap v -> FieldMap v -> FieldMap v
union = unionWith const

unionWith :: (v -> v -> v) -> FieldMap v -> FieldMap v -> FieldMap v
unionWith f l r = alignWith (these id id f) l r

foldMapWithKey :: Monoid m => (String -> a -> m) -> FieldMap a -> m
foldMapWithKey f (FieldMap lm) = foldMap (uncurry f) lm

instance Semigroup v => Semigroup (FieldMap v) where
    (<>) = unionWith (<>)

instance Semigroup v => Monoid (FieldMap v) where
    mempty = FieldMap mempty

instance ToJSON v => ToJSON (FieldMap v) where
    toJSON (FieldMap m) = JsonObject [(k, toJSON v) | (k, v) <- m]

instance Pretty a => PrettyFieldClass (FieldMap a) where
    prettyField (FieldMap m) =
        [ PrettyField () (toUTF8BS n) (pretty a) | (n, a) <- m
        ]

instance Pretty a => Pretty (FieldMap a) where
    pretty = text . showFields (const NoComment) . prettyField

pop :: Eq a => a -> [(a, b)] -> (Maybe b, [(a, b)])
pop n l = (fmap snd (safeHead matches), rest)
  where
    (matches, rest) = partition ((n ==) . fst) l
