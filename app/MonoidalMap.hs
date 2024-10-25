{-# LANGUAGE CPP #-}
module MonoidalMap where

import Data.List
    ( insertBy
#if !MIN_VERSION_base(4, 20, 0)
    , foldl'
#endif
    )
import Data.List.NonEmpty qualified as NE
import Data.Ord (comparing)
import Json (Json (..), ToJSON (..))

newtype MonoidalMap k v = MonoidalMap {unMonoidalMap :: [(k, v)]}
    deriving (Show, Functor, Foldable, Traversable)

instance (Ord k, Monoid v) => Semigroup (MonoidalMap k v) where
    MonoidalMap lhs <> MonoidalMap rhs =
        MonoidalMap $
            map (\ne -> (fst (NE.head ne), foldMap snd ne)) $
                NE.groupWith fst $
                    foldl' (flip $ insertBy (comparing fst)) [] $
                        lhs ++ rhs

instance (Ord k, Monoid v) => Monoid (MonoidalMap k v) where
    mempty = MonoidalMap mempty

instance ToJSON v => ToJSON (MonoidalMap String v) where
    toJSON (MonoidalMap m) = JsonObject [(k, toJSON v) | (k, v) <- m]
