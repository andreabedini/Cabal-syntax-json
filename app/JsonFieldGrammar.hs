module JsonFieldGrammar where

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

--
-- FieldGrammar stuff
--

newtype JSONFieldGrammar s a = JsonFG
    { fieldGrammarJSON :: CabalSpecVersion -> s -> [(String, JsonFragment)]
    }
    deriving Functor

jsonFieldGrammar :: CabalSpecVersion -> JSONFieldGrammar s a -> s -> [(String, JsonFragment)]
jsonFieldGrammar v fg = fieldGrammarJSON fg v

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
        single . scalarFragment fn . toJSON . pack' _pack . aview l

    booleanFieldDef
        :: FieldName
        -> ALens' s Bool
        -> Bool
        -> JSONFieldGrammar s Bool
    booleanFieldDef fn l def = JsonFG $ \_ s ->
        let b = aview l s
         in if b == def
                then mempty
                else single (scalarFragment fn (toJSON b))

    optionalFieldAla
        :: (ToJSON b, Newtype a b)
        => FieldName
        -> (a -> b)
        -> ALens' s (Maybe a)
        -> JSONFieldGrammar s (Maybe a)
    optionalFieldAla fn _pack l = JsonFG $ \_ ->
        maybe mempty (single . scalarFragment fn . toJSON . pack' _pack) . aview l

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
                else single (scalarFragment fn (toJSON (pack' _pack x)))

    freeTextField
        :: FieldName
        -> ALens' s (Maybe String)
        -> JSONFieldGrammar s (Maybe String)
    freeTextField fn l = JsonFG $ \_ ->
        maybe mempty (single . scalarFragment fn . toJSON) . aview l

    freeTextFieldDef
        :: FieldName
        -> ALens' s String
        -> JSONFieldGrammar s String
    freeTextFieldDef fn l = JsonFG $ \_ s ->
        let x = aview l s
         in if x == ""
                then mempty
                else single (scalarFragment fn (JsonString x))

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
            JsonArray [] -> mempty
            JsonArray js -> single (listlikeFragment fn js)
            j -> single (scalarFragment fn j)

    prefixedFields
        :: FieldName
        -> ALens' s [(String, String)]
        -> JSONFieldGrammar s [(String, String)]
    prefixedFields _fnPfx l = JsonFG $ \_ s ->
        [scalarFragment' n (JsonString v) | (n, v) <- aview l s]

    knownField
        :: FieldName
        -> JSONFieldGrammar s ()
    knownField _ = pure ()

    deprecatedSince
        :: CabalSpecVersion
        -> String
        -> JSONFieldGrammar s a
        -> JSONFieldGrammar s a
    deprecatedSince _ _ x = x

    -- TODO: as PrettyFieldGrammar isn't aware of cabal-version: we output the field
    -- this doesn't affect roundtrip as `removedIn` fields cannot be parsed
    -- so invalid documents can be only manually constructed.
    removedIn _ _ x = x

    availableSince _ _ = id

    hiddenField _ = JsonFG (const mempty)

single :: (String, JsonFragment) -> [(String, JsonFragment)]
single = pure

data Fragment a
    = ScalarFragment a
    | ListLikeFragment [a]
    deriving (Show, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Fragment a) where
    toJSON (ScalarFragment j) = toJSON j
    toJSON (ListLikeFragment js) = JsonArray $ map toJSON js
    toJSONList vs = JsonArray $ flip foldMap vs $ \case
        ScalarFragment j -> [toJSON j]
        ListLikeFragment js -> map toJSON js

type JsonFragment = Fragment Json

scalarFragment :: FieldName -> Json -> (String, JsonFragment)
scalarFragment fn v = (fromUTF8BS fn, ScalarFragment v)

scalarFragment' :: String -> Json -> (String, JsonFragment)
scalarFragment' fn v = (fn, ScalarFragment v)

listlikeFragment :: FieldName -> [Json] -> (String, JsonFragment)
listlikeFragment fn vs = (fromUTF8BS fn, ListLikeFragment vs)
