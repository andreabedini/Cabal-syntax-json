-- | Our 'FieldGrammar' interpretation, which renders each cabal field to a JSON
-- 'Fragment', together with the 'Fragment' type itself.
--
-- @Cabal@ describes each field group (package description, library, executable, …)
-- once, as a 'FieldGrammar', and reuses that description for parsing, pretty-printing,
-- and more. 'JSONFieldGrammar' is one more interpretation of the same descriptions:
-- run it over a value and it yields that value's fields as @(name, 'Fragment' 'Json')@
-- pairs — no per-field code on our side.
module Cabal.Syntax.JsonFieldGrammar where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE

import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Compat.Lens (ALens', aview)
import Distribution.Compat.Newtype (Newtype, alaf, pack')
import Distribution.FieldGrammar (FieldGrammar (..), defaultFreeTextFieldDefST)
import Distribution.Fields (FieldName)
import Distribution.Pretty (Pretty (..))
import Distribution.Utils.Generic (fromUTF8BS, fromUTF8LBS)
import Distribution.Utils.Json (Json (..), renderJson)
import Distribution.Utils.ShortText qualified as ST

import Text.PrettyPrint (Doc, text)

import Cabal.Syntax.Json (ToJSON (..))
import Cabal.Syntax.Pretty (Vertically (..))

--
-- FieldGrammar stuff
--

-- | A 'FieldGrammar' interpretation. Given the cabal spec version and a source value
-- @s@, it produces that value's fields as @(name, 'Fragment' 'Json')@ pairs. The
-- result type @a@ is a phantom carried only to satisfy the 'FieldGrammar' interface.
newtype JSONFieldGrammar s a = JsonFG
    { runJsonFieldGrammar :: CabalSpecVersion -> s -> [(String, Fragment Json)]
    }
    deriving Functor

-- | Run a 'JSONFieldGrammar', yielding the @(field-name, fragment)@ pairs of @s@.
jsonFieldGrammar :: CabalSpecVersion -> JSONFieldGrammar s a -> s -> [(String, Fragment Json)]
jsonFieldGrammar v fg = runJsonFieldGrammar fg v

-- | Like 'jsonFieldGrammar', but assemble the fields into a single JSON object.
jsonFieldGrammar' :: CabalSpecVersion -> JSONFieldGrammar s a -> s -> Json
jsonFieldGrammar' v fg s = JsonObject [(n, toJSON a) | (n, a) <- jsonFieldGrammar v fg s]

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
        pure . scalarFragment fn . toJSON . pack' _pack . aview l

    booleanFieldDef
        :: FieldName
        -> ALens' s Bool
        -> Bool
        -> JSONFieldGrammar s Bool
    booleanFieldDef fn l def = JsonFG $ \_ s ->
        let b = aview l s
         in if b == def
                then mempty
                else [scalarFragment fn (toJSON b)]

    optionalFieldAla
        :: (ToJSON b, Newtype a b)
        => FieldName
        -> (a -> b)
        -> ALens' s (Maybe a)
        -> JSONFieldGrammar s (Maybe a)
    optionalFieldAla fn _pack l = JsonFG $ \_ ->
        maybe mempty (pure . scalarFragment fn . toJSON . pack' _pack) . aview l

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
                else [scalarFragment fn (toJSON (pack' _pack x))]

    freeTextField
        :: FieldName
        -> ALens' s (Maybe String)
        -> JSONFieldGrammar s (Maybe String)
    freeTextField fn l = JsonFG $ \_ ->
        maybe mempty (pure . scalarFragment fn . toJSON) . aview l

    freeTextFieldDef
        :: FieldName
        -> ALens' s String
        -> JSONFieldGrammar s String
    freeTextFieldDef fn l = JsonFG $ \_ s ->
        let x = aview l s
         in if x == ""
                then mempty
                else [scalarFragment fn (JsonString x)]

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
            JsonArray js ->
                case NE.nonEmpty js of
                    Nothing -> []
                    Just ne -> [listlikeFragment fn ne]
            j -> [listlikeFragment fn (NE.singleton j)]

    prefixedFields
        :: FieldName
        -> ALens' s [(String, String)]
        -> JSONFieldGrammar s [(String, String)]
    prefixedFields fnPfx l = JsonFG $ \_ kv ->
        foldMap
            (\(fn, s) -> [scalarFragment' (fromUTF8BS fnPfx ++ fn) (JsonString s)])
            (aview l kv)

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

-- | A single field's value, tagged with whether the field is a scalar or a sequence.
--
-- == Why not just 'Json'?
--
-- Once conditionals are pushed inside fields (see "Cabal.Syntax.Pipeline"), one field
-- can collect values from several @if@\/@else@ branches that must be /merged/ — and how
-- they merge depends on the field's arity:
--
--   * a scalar field (e.g. @name@, @version@) carries a 'ScalarFragment' and is
--     replaced, never combined;
--   * a list-like field (e.g. @build-depends@, @exposed-modules@) carries a
--     'ListLikeFragment' and /concatenates/ across branches.
--
-- Keeping that distinction is what lets 'Cabal.Syntax.CondTree.defragment' splice a
-- guard /into/ a list field — @[v1, {_if:…, _then:[v2]}]@ — instead of overwriting it.
-- The 'Semigroup' instance encodes the same idea: combining any two fragments yields a
-- 'ListLikeFragment', because a value that appears under more than one condition is, by
-- construction, a sequence.
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

instance Pretty (Fragment Json) where
    pretty (ScalarFragment a) =
        prettyJson a
    pretty (ListLikeFragment as) =
        alaf Vertically foldMap prettyJson as

-- | Render a 'Json' value to a single-line 'Doc' (used by the @--debug@ printer).
prettyJson :: Json -> Doc
prettyJson = text . fromUTF8LBS . renderJson

-- | A @(field-name, fragment)@ pair for a scalar field, decoding the 'FieldName' to a
-- 'String'.
scalarFragment :: FieldName -> a -> (String, Fragment a)
scalarFragment fn v = (fromUTF8BS fn, ScalarFragment v)

-- | Like 'scalarFragment' but taking the field name already as a 'String'.
scalarFragment' :: String -> a -> (String, Fragment a)
scalarFragment' fn v = (fn, ScalarFragment v)

-- | A @(field-name, fragment)@ pair for a list-like field.
listlikeFragment :: FieldName -> NonEmpty a -> (String, Fragment a)
listlikeFragment fn vs = (fromUTF8BS fn, ListLikeFragment vs)
