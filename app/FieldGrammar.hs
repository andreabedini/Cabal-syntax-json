module FieldGrammar where

import Data.Foldable (Foldable (..))
import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Compat.Lens (ALens', aview)
import Distribution.Compat.Newtype (Newtype, pack')
import Distribution.FieldGrammar
    ( FieldGrammar (..)
    , defaultFreeTextFieldDefST
    )
import Distribution.Fields (FieldName)
import Distribution.PackageDescription
    ( CondBranch (..)
    , CondTree (..)
    , Condition (..)
    , ConfVar
    , FlagName
    , GenericPackageDescription (..)
    , LibraryName (..)
    , PackageDescription (..)
    , PackageFlag (..)
    , SetupBuildInfo
    , SourceRepo (..)
    , UnqualComponentName
    , cAnd
    , cNot
    , mapTreeData
    , unFlagName
    , unUnqualComponentName
    )
import Distribution.PackageDescription.FieldGrammar
    ( benchmarkFieldGrammar
    , executableFieldGrammar
    , flagFieldGrammar
    , foreignLibFieldGrammar
    , libraryFieldGrammar
    , packageDescriptionFieldGrammar
    , setupBInfoFieldGrammar
    , sourceRepoFieldGrammar
    , testSuiteFieldGrammar
    , unvalidateBenchmark
    , unvalidateTestSuite
    )
import Distribution.Utils.Generic (fromUTF8BS)
import Distribution.Utils.Json
    ( Json (..)
    , (.=)
    )
import Distribution.Utils.ShortText qualified as ST
import Json (Pair, ToJSON (..))
import MonoidalMap (MonoidalMap (..))

type FieldMap a = MonoidalMap String a

data Value a
    = ScalarValue a
    | ListLikeValue [a]
    deriving (Show, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Value a) where
    toJSON (ScalarValue j) = toJSON j
    toJSON (ListLikeValue js) = JsonArray $ map toJSON js
    toJSONList vs = JsonArray $ flip foldMap vs $ \case
        ScalarValue j -> [toJSON j]
        ListLikeValue js -> map toJSON js

type JsonValue = Value Json

data CJson = CJson (Condition ConfVar) Json
    deriving Show

instance ToJSON CJson where
    toJSON (CJson (Lit True) v) = v
    toJSON (CJson c v) = JsonObject ["_if" .= toJSON c, "_then" .= v]

scalarField :: FieldName -> Json -> [(String, Value Json)]
scalarField fn v = [(fromUTF8BS fn, ScalarValue v)]

listlikeField :: FieldName -> [Json] -> [(String, Value Json)]
listlikeField fn vs = [(fromUTF8BS fn, ListLikeValue vs)]

jsonGenericPackageDescription :: GenericPackageDescription -> Json
jsonGenericPackageDescription gpd =
    JsonObject $
        concat
            [ disolve $ jsonFieldGrammar v packageDescriptionFieldGrammar $ packageDescription gpd
            , ["source-repos" .= jsonSourceRepos v repos | let repos = sourceRepos (packageDescription gpd), not (null repos)]
            , maybe [] (\s -> [("custom-setup", toJSON $ jsonSetupBInfo v s)]) $ setupBuildInfo (packageDescription gpd)
            , ["flags" .= jsonPackageFlags v flags | let flags = genPackageFlags gpd, not (null flags)]
            , maybe [] (\l -> [("library", toJSON $ jsonCondTree v (libraryFieldGrammar LMainLibName) l)]) $ condLibrary gpd
            , named "sub-libraries" $ jsonCondTreeMap v (libraryFieldGrammar . LSubLibName) $ condSubLibraries gpd
            , named "foreign-libraries" $ jsonCondTreeMap v foreignLibFieldGrammar $ condForeignLibs gpd
            , named "executables" $ jsonCondTreeMap v executableFieldGrammar $ condExecutables gpd
            , named "test-suites" $ jsonCondTreeMap v (const testSuiteFieldGrammar) $ fmap (fmap (mapTreeData unvalidateTestSuite)) $ condTestSuites gpd
            , named "benchmarks" $ jsonCondTreeMap v (const benchmarkFieldGrammar) $ fmap (fmap (mapTreeData unvalidateBenchmark)) $ condBenchmarks gpd
            ]
  where
    v = specVersion $ packageDescription gpd

disolve :: [(String, Value Json)] -> [(String, Json)]
disolve pairs = [(n, toJSON v) | (n, v) <- pairs]

jsonCondTreeMap
    :: CabalSpecVersion
    -> (UnqualComponentName -> JSONFieldGrammar s s)
    -> [(UnqualComponentName, CondTree ConfVar c s)]
    -> [(String, Json)]
jsonCondTreeMap ver fg =
    foldMap
        ( \(k, v) ->
            [ (unUnqualComponentName k, toJSON $ jsonCondTree ver (fg k) v)
            ]
        )

jsonCondTree
    :: CabalSpecVersion
    -> JSONFieldGrammar s s
    -> CondTree ConfVar c s
    -> FieldMap [Value CJson]
jsonCondTree ver fg =
    go (Lit True) . mapTreeData (MonoidalMap . jsonFieldGrammar ver fg)
  where
    go :: Condition ConfVar -> CondTree ConfVar c (FieldMap (Value Json)) -> FieldMap [Value CJson]
    go c (CondNode it _ ifs) =
        fmap (traverse $ \j -> [CJson c j]) it <> foldMap (goBranch c) ifs

    goBranch :: Condition ConfVar -> CondBranch ConfVar c (FieldMap (Value Json)) -> FieldMap [Value CJson]
    goBranch c1 (CondBranch c2 thenTree elseTree) =
        go (c1 `cAnd` c2) thenTree <> foldMap (go (c1 `cAnd` cNot c2)) elseTree

jsonSourceRepos :: CabalSpecVersion -> [SourceRepo] -> Json
jsonSourceRepos v = JsonArray . fmap (toJSON . MonoidalMap . jsonSourceRepo v)

jsonPackageFlags :: CabalSpecVersion -> [PackageFlag] -> Json
jsonPackageFlags v flags =
    JsonObject
        [ unFlagName name .= toJSON (MonoidalMap $ jsonPackageFlag v name flag)
        | flag@(MkPackageFlag name _ _ _) <- flags
        ]

jsonPackageFlag :: CabalSpecVersion -> FlagName -> PackageFlag -> [(String, Value Json)]
jsonPackageFlag v name =
    jsonFieldGrammar v (flagFieldGrammar name)

jsonSourceRepo :: CabalSpecVersion -> SourceRepo -> [(String, Value Json)]
jsonSourceRepo v repo =
    jsonFieldGrammar v (sourceRepoFieldGrammar (repoKind repo)) repo

jsonSetupBInfo :: CabalSpecVersion -> SetupBuildInfo -> [(String, Value Json)]
jsonSetupBInfo v sbi =
    jsonFieldGrammar v (setupBInfoFieldGrammar False) sbi

newtype JSONFieldGrammar s a = JsonFG
    { fieldGrammarJSON :: CabalSpecVersion -> s -> [(String, JsonValue)]
    }
    deriving Functor

jsonFieldGrammar :: CabalSpecVersion -> JSONFieldGrammar s a -> s -> [(String, Value Json)]
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
        scalarField fn . toJSON . pack' _pack . aview l

    booleanFieldDef
        :: FieldName
        -> ALens' s Bool
        -> Bool
        -> JSONFieldGrammar s Bool
    booleanFieldDef fn l def = JsonFG $ \_ s ->
        let b = aview l s
         in if b == def
                then mempty
                else scalarField fn (toJSON b)

    optionalFieldAla
        :: (ToJSON b, Newtype a b)
        => FieldName
        -> (a -> b)
        -> ALens' s (Maybe a)
        -> JSONFieldGrammar s (Maybe a)
    optionalFieldAla fn _pack l = JsonFG $ \_ ->
        maybe mempty (scalarField fn . toJSON . pack' _pack) . aview l

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
                else scalarField fn (toJSON (pack' _pack x))

    freeTextField
        :: FieldName
        -> ALens' s (Maybe String)
        -> JSONFieldGrammar s (Maybe String)
    freeTextField fn l = JsonFG $ \_ ->
        maybe mempty (scalarField fn . toJSON) . aview l

    freeTextFieldDef
        :: FieldName
        -> ALens' s String
        -> JSONFieldGrammar s String
    freeTextFieldDef fn l = JsonFG $ \_ s ->
        let x = aview l s
         in if x == ""
                then mempty
                else scalarField fn (JsonString x)

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
            JsonArray js -> listlikeField fn js
            j -> scalarField fn j

    prefixedFields
        :: FieldName
        -> ALens' s [(String, String)]
        -> JSONFieldGrammar s [(String, String)]
    prefixedFields _fnPfx l = JsonFG $ \_ s ->
        [(n, ScalarValue (JsonString v)) | (n, v) <- aview l s]

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

named :: Foldable t => String -> t Pair -> [Pair]
named key xs = [key .= JsonObject (toList xs) | not (null xs)]
