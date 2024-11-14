module JsonFieldGrammar where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Compat.Lens (ALens', aview)
import Distribution.Compat.Newtype (Newtype, pack')
import Distribution.FieldGrammar
    ( FieldGrammar (..)
    , defaultFreeTextFieldDefST
    )
import Distribution.Fields (FieldName)
import Distribution.Utils.Generic (fromUTF8BS)
import Distribution.Utils.Json (Json (..))
import Distribution.Utils.ShortText qualified as ST
import Json (ToJSON (..))
import MonoidalMap (FieldMap, singleton)

--
-- FieldGrammar stuff
--

newtype JSONFieldGrammar s a = JsonFG
    { runJsonFieldGrammar :: CabalSpecVersion -> s -> FieldMap (Fragment Json)
    }
    deriving Functor

jsonFieldGrammar :: CabalSpecVersion -> JSONFieldGrammar s a -> s -> FieldMap (Fragment Json)
jsonFieldGrammar v fg = runJsonFieldGrammar fg v

instance Applicative (JSONFieldGrammar s) where
    pure _ = JsonFG (\_ _ -> mempty)
    JsonFG f <*> JsonFG x = JsonFG (\v s -> f v s <> x v s)

instance FieldGrammar ToJSON JSONFieldGrammar where
    blurFieldGrammar
        :: ALens' a b
        -> JSONFieldGrammar b d
        -> JSONFieldGrammar a d
    blurFieldGrammar f (JsonFG fg) = JsonFG $ \v ->
        fg v . aview f

    uniqueFieldAla
        :: (ToJSON b, Newtype a b)
        => FieldName
        -> (a -> b)
        -> ALens' s a
        -> JSONFieldGrammar s a
    uniqueFieldAla fn _pack l = JsonFG $ \_ ->
        scalarFragment fn . toJSON . pack' _pack . aview l

    booleanFieldDef
        :: FieldName
        -> ALens' s Bool
        -> Bool
        -> JSONFieldGrammar s Bool
    booleanFieldDef fn l def = JsonFG $ \_ s ->
        let b = aview l s
         in if b == def
                then mempty
                else scalarFragment fn (toJSON b)

    optionalFieldAla
        :: (ToJSON b, Newtype a b)
        => FieldName
        -> (a -> b)
        -> ALens' s (Maybe a)
        -> JSONFieldGrammar s (Maybe a)
    optionalFieldAla fn _pack l = JsonFG $ \_ ->
        maybe mempty (scalarFragment fn . toJSON . pack' _pack) . aview l

    optionalFieldDefAla
        :: (ToJSON b, Newtype a b, Eq a)
        => FieldName
        -> (a -> b)
        -> ALens' s a
        -> a
        -> JSONFieldGrammar s a
    optionalFieldDefAla fn _pack l def = JsonFG $ \_ s ->
        let x = aview l s
         in if x == def
                then mempty
                else scalarFragment fn (toJSON (pack' _pack x))

    freeTextField
        :: FieldName
        -> ALens' s (Maybe String)
        -> JSONFieldGrammar s (Maybe String)
    freeTextField fn l = JsonFG $ \_ ->
        maybe mempty (scalarFragment fn . toJSON) . aview l

    freeTextFieldDef
        :: FieldName
        -> ALens' s String
        -> JSONFieldGrammar s String
    freeTextFieldDef fn l = JsonFG $ \_ s ->
        let x = aview l s
         in if x == ""
                then mempty
                else scalarFragment fn (JsonString x)

    freeTextFieldDefST
        :: FieldName
        -> ALens' s ST.ShortText
        -> JSONFieldGrammar s ST.ShortText
    freeTextFieldDefST = defaultFreeTextFieldDefST

    monoidalFieldAla
        :: (ToJSON b, Monoid a, Newtype a b)
        => FieldName
        -> (a -> b)
        -> ALens' s a
        -> JSONFieldGrammar s a
    monoidalFieldAla fn _pack l = JsonFG $ \_ s ->
        case toJSON (pack' _pack $ aview l s) of
            JsonArray js -> foldMap (listlikeFragment fn) (NE.nonEmpty js)
            j -> listlikeFragment fn (NE.singleton j)

    prefixedFields
        :: FieldName
        -> ALens' s [(String, String)]
        -> JSONFieldGrammar s [(String, String)]
    prefixedFields fnPfx l = JsonFG $ \_ kv ->
        mconcat [scalarFragment' (fromUTF8BS fnPfx ++ fn) (JsonString s) | (fn, s) <- aview l kv]

    knownField :: FieldName -> JSONFieldGrammar s ()
    knownField _fn =
        pure ()

    deprecatedSince
        :: CabalSpecVersion
        -> String
        -> JSONFieldGrammar s a
        -> JSONFieldGrammar s a
    deprecatedSince _v _fn fg = fg

    hiddenField
        :: JSONFieldGrammar s a
        -> JSONFieldGrammar s a
    hiddenField fg = fg

    removedIn
        :: CabalSpecVersion
        -> String
        -> JSONFieldGrammar s a
        -> JSONFieldGrammar s a
    removedIn _v _fn fg = fg

    availableSince
        :: CabalSpecVersion
        -> a
        -> JSONFieldGrammar s a
        -> JSONFieldGrammar s a
    availableSince _v _a fg = fg

data Fragment a
    = ScalarFragment a
    | ListLikeFragment (NonEmpty a)
    deriving (Show, Functor, Foldable, Traversable)

instance Semigroup (Fragment a) where
    ScalarFragment a <> ScalarFragment b = ListLikeFragment (a :| [b])
    ScalarFragment a <> ListLikeFragment bs = ListLikeFragment (NE.singleton a <> bs)
    ListLikeFragment as <> ScalarFragment b = ListLikeFragment (as <> NE.singleton b)
    ListLikeFragment as <> ListLikeFragment bs = ListLikeFragment (as <> bs)

instance ToJSON a => ToJSON (Fragment a) where
    toJSON (ScalarFragment j) = toJSON j
    toJSON (ListLikeFragment js) = toJSON js
    toJSONList =
        JsonArray
            . ( foldMap $ \case
                    ScalarFragment j -> [toJSON j]
                    ListLikeFragment js -> NE.toList (NE.map toJSON js)
              )

type JsonFragment = Fragment Json

scalarFragment :: FieldName -> a -> FieldMap (Fragment a)
scalarFragment fn v = singleton (fromUTF8BS fn) (ScalarFragment v)

scalarFragment' :: String -> a -> FieldMap (Fragment a)
scalarFragment' fn v = singleton fn (ScalarFragment v)

listlikeFragment :: FieldName -> NonEmpty a -> FieldMap (Fragment a)
listlikeFragment fn vs = singleton (fromUTF8BS fn) (ListLikeFragment vs)
