{-# LANGUAGE FunctionalDependencies #-}

module Cabal.Syntax.Utils
    ( -- * with index
      FoldableWithIndex (..)

      -- * These
    , These (..)
    , these
    , justHere
    , justThere

      -- * Semialign
    , Semialign (..)
    , Align (..)
    ) where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifoldable1 (Bifoldable1 (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))

class FoldableWithIndex i f | f -> i where
    itoList :: f a -> [(i, a)]
    ifoldMap :: Monoid m => (i -> a -> m) -> f a -> m

data These a b = This a | That b | These a b
    deriving (Eq, Show)

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

instance Bifunctor These where
    bimap f _ (This a) = This (f a)
    bimap _ g (That x) = That (g x)
    bimap f g (These a x) = These (f a) (g x)

instance Bifoldable These where
    bifoldMap f g = these f g (\x y -> mappend (f x) (g y))

instance Bifoldable1 These where
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

class Functor f => Semialign f where
    align :: f a -> f b -> f (These a b)
    align = alignWith id

    alignWith :: (These a b -> c) -> f a -> f b -> f c
    alignWith f a b = f <$> align a b

    {-# MINIMAL (align | alignWith) #-}

class Semialign f => Align f where
    nil :: f a
