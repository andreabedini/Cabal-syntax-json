{-# LANGUAGE DerivingVia #-}

module FieldMap
    ( FieldMap
    , singleton
    , toList
    , fromList
    , empty
    , unionWith
    , foldMapWithKey
    , These (..)
    , these
    , FieldMap.lookup
    , align
    , alignWith
    , justHere
    , justThere
    ) where

import Distribution.Fields.Pretty (CommentPosition (..), PrettyField (..), showFields)
import Distribution.Pretty (Pretty (..))
import Distribution.Utils.Generic (safeHead, toUTF8BS)
import Distribution.Utils.Json (Json (..))

import Text.PrettyPrint (text)

import Data.Bifoldable
import Data.Bifoldable1
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable1 (Foldable1 (..))
import Data.Function (on)
import Data.List (partition)
import Data.List.NonEmpty qualified as NE
import Json (ToJSON (..))
import Pretty (PrettyFieldClass (..))

newtype FieldMap v = FieldMap [(String, v)]
    deriving (Show, Functor, Foldable, Traversable)

singleton :: String -> v -> FieldMap v
singleton k v = FieldMap [(k, v)]

toList :: FieldMap v -> [(String, v)]
toList (FieldMap m) = m

fromList :: Semigroup v => [(String, v)] -> FieldMap v
fromList =
    FieldMap
        . map (\ne -> (fst (NE.head ne), foldMap1 snd ne))
        . NE.groupBy ((==) `on` fst)

empty :: FieldMap v
empty = FieldMap mempty

lookup :: FieldMap v -> String -> Maybe v
lookup (FieldMap m) n = Prelude.lookup n m

pop :: Eq a => a -> [(a, b)] -> (Maybe b, [(a, b)])
pop n l = (fmap snd (safeHead matches), rest)
  where
    (matches, rest) = partition ((n ==) . fst) l

unionWith :: (v -> v -> v) -> FieldMap v -> FieldMap v -> FieldMap v
unionWith f (FieldMap lm) (FieldMap rm) =
    FieldMap
        [ (n, maybe l (f l) (Prelude.lookup n rm))
        | (n, l) <- lm
        ]

-- merge :: Eq a => (b -> b -> b) -> [(a, b)] -> [(a, b)] -> [(a, b)]
-- merge f = go
--   where
--     go ((ln, lv) : ls) rs =
--         let (matches, rest) = partition ((ln ==) . fst) rs
--          in (ln, foldr f lv (map snd matches)) : go ls rest
--     go [] rs = rs

data These a b = This a | That b | These a b
    deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (These a b) where
    This a <> This b = This (a <> b)
    This a <> That y = These a y
    This a <> These b y = These (a <> b) y
    That x <> This b = These b x
    That x <> That y = That (x <> y)
    That x <> These b y = These b (x <> y)
    These a x <> This b = These (a <> b) x
    These a x <> That y = These a (x <> y)
    These a x <> These b y = These (a <> b) (x <> y)

instance Functor (These a) where
    fmap _ (This x) = This x
    fmap f (That y) = That (f y)
    fmap f (These x y) = These x (f y)

instance Foldable (These a) where
    foldr _ z (This _) = z
    foldr f z (That x) = f x z
    foldr f z (These _ x) = f x z

instance Traversable (These a) where
    traverse _ (This a) = pure $ This a
    traverse f (That x) = That <$> f x
    traverse f (These a x) = These a <$> f x
    sequenceA (This a) = pure $ This a
    sequenceA (That x) = That <$> x
    sequenceA (These a x) = These a <$> x

instance Bifunctor These where
    bimap f _ (This a) = This (f a)
    bimap _ g (That x) = That (g x)
    bimap f g (These a x) = These (f a) (g x)

instance Bifoldable These where
    bifold = these id id mappend
    bifoldMap f g = these f g (\x y -> mappend (f x) (g y))
    bifoldr f g z = these (`f` z) (`g` z) (\x y -> x `f` (y `g` z))
    bifoldl f g z = these (z `f`) (z `g`) (\x y -> (z `f` x) `g` y)

instance Bifoldable1 These where
    bifold1 = these id id (<>)
    bifoldMap1 f g = these f g (\x y -> f x <> g y)

instance Bitraversable These where
    bitraverse f _ (This x) = This <$> f x
    bitraverse _ g (That x) = That <$> g x
    bitraverse f g (These x y) = These <$> f x <*> g y

these :: (a -> c) -> (b -> c) -> (a -> b -> c) -> These a b -> c
these f _ _ (This a) = f a
these _ g _ (That b) = g b
these _ _ h (These a b) = h a b

justHere :: These a b -> Maybe a
justHere (This a) = Just a
justHere (That _) = Nothing
justHere (These a _) = Just a

justThere :: These a b -> Maybe b
justThere (This _) = Nothing
justThere (That b) = Just b
justThere (These _ b) = Just b

align :: FieldMap b -> FieldMap c -> FieldMap (These b c)
align (FieldMap lm) (FieldMap rm) = FieldMap $ go lm rm
  where
    go :: Eq a => [(a, b)] -> [(a, c)] -> [(a, These b c)]
    go ((ln, lv) : ls) rs =
        case pop ln rs of
            (Nothing, rest) ->
                (ln, This lv) : go ls rest
            (Just rv, rest) ->
                (ln, These lv rv) : go ls rest
    go [] rs = [(rn, That rv) | (rn, rv) <- rs]

alignWith :: (These b c -> v) -> FieldMap b -> FieldMap c -> FieldMap v
alignWith f lm rm = fmap f $ align lm rm

-- align :: forall a b c. Eq a => [(a, b)] -> [(a, c)] -> [(a, These b c)]
-- align = go
--   where
--     go :: Eq a => [(a, b)] -> [(a, c)] -> [(a, These b c)]
--     go ((ln, lv) : ls) rs =
--         case partition ((ln ==) . fst) rs of
--             ([], rest) ->
--                 (ln, This lv) : go ls rest
--             ((_, rv) : _ignored, rest) ->
--                 (ln, These lv rv) : go ls rest
--     go [] rs = [(rn, That rv) | (rn, rv) <- rs]

foldMapWithKey :: Monoid m => (String -> a -> m) -> FieldMap a -> m
foldMapWithKey f (FieldMap lm) = foldMap (uncurry f) lm

instance Semigroup v => Semigroup (FieldMap v) where
    (<>) = unionWith (<>)

instance Semigroup v => Monoid (FieldMap v) where
    mempty = FieldMap mempty

instance ToJSON v => ToJSON (FieldMap v) where
    toJSON (FieldMap m) = JsonObject [(k, toJSON v) | (k, v) <- m]

instance Pretty a => PrettyFieldClass (FieldMap a) where
    prettyField (FieldMap m) = [PrettyField () (toUTF8BS n) (pretty a) | (n, a) <- m]

instance Pretty a => Pretty (FieldMap a) where
    pretty = text . showFields (const NoComment) . prettyField

-- deriving via Map String instance Semialign FieldMap
-- deriving via Map String instance Align FieldMap

-- .merge (Map.mapMissing (\_ x ->  f (This x)))
--                             (Map.mapMissing (\_ y ->  f (That y)))
--                             (Map.zipWithMatched (\_ x y -> f (These x y)))
