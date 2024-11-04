module FieldGrammar (jsonGenericPackageDescription) where

import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import Data.Maybe (fromMaybe)
import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Compat.Lens (ALens', aview)
import Distribution.Compat.Newtype (Newtype, pack')
import Distribution.FieldGrammar
    ( FieldGrammar (..)
    , defaultFreeTextFieldDefST
    )
import Distribution.Fields (FieldName)
import Distribution.Package (Package (..))
import Distribution.PackageDescription
    ( CondBranch (..)
    , CondTree (..)
    , Condition (..)
    , ConfVar
    , GenericPackageDescription (..)
    , LibraryName (..)
    , PackageDescription (..)
    , PackageFlag (..)
    , PackageIdentifier (..)
    , SetupBuildInfo
    , SourceRepo (..)
    , cAnd
    , cNot
    , libraryNameString
    , mapTreeData
    , unFlagName
    , unPackageName
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
import Distribution.Pretty (prettyShow)
import Distribution.Utils.Generic (fromUTF8BS)
import Distribution.Utils.Json (Json (..), (.=))
import Distribution.Utils.ShortText qualified as ST
import Json (ToJSON (..))
import MonoidalMap (MonoidalMap (..))

jsonGenericPackageDescription :: GenericPackageDescription -> Json
jsonGenericPackageDescription gpd =
    JsonObject $
        concat
            [ map (second toJSON) $ jsonFieldGrammar v packageDescriptionFieldGrammar (packageDescription gpd)
            , [ "source-repos" .= toJSON (map (jsonSourceRepo v) repos)
              | let repos = sourceRepos (packageDescription gpd)
              , not (null repos)
              ]
            , [ "custom-setup" .= jsonCustomSetup v sbi
              | sbi <- toList (setupBuildInfo (packageDescription gpd))
              ]
            , [ "flags" .= JsonObject [unFlagName (flagName flag) .= jsonFlag v flag | flag <- flags]
              | let flags = genPackageFlags gpd
              , not (null flags)
              ]
            , [ "libraries"
                    .= JsonObject
                        [ (libraryName ln, toJSON (jsonCondTree v (libraryFieldGrammar ln) l))
                        | (ln, l) <- libraries
                        ]
              | not (null libraries)
              ]
            , [ "foreign-libraries"
                    .= JsonObject
                        [ prettyShow ucn .= toJSON (jsonCondTree v (foreignLibFieldGrammar ucn) c)
                        | (ucn, c) <- flibs
                        ]
              | let flibs = condForeignLibs gpd
              , not (null flibs)
              ]
            , [ "executables"
                    .= JsonObject
                        [ prettyShow ucn .= toJSON (jsonCondTree v (executableFieldGrammar ucn) c)
                        | (ucn, c) <- exes
                        ]
              | let exes = condExecutables gpd
              , not (null exes)
              ]
            , [ "test-suites"
                    .= JsonObject
                        [ prettyShow ucn .= toJSON (jsonCondTree v testSuiteFieldGrammar c)
                        | (ucn, c) <- testSuites
                        ]
              | not (null testSuites)
              ]
            , [ "benchmarks"
                    .= JsonObject
                        [ prettyShow ucn .= toJSON (jsonCondTree v benchmarkFieldGrammar c)
                        | (ucn, c) <- benchmarks
                        ]
              | not (null benchmarks)
              ]
            ]
  where
    pn = pkgName $ packageId $ packageDescription gpd
    libraryName = maybe (unPackageName pn) unUnqualComponentName . libraryNameString
    v = specVersion $ packageDescription gpd
    libraries =
        mconcat
            [ [(LMainLibName, l) | l <- toList (condLibrary gpd)]
            , [(LSubLibName ucn, l) | (ucn, l) <- condSubLibraries gpd]
            ]
    testSuites = map (fmap (mapTreeData unvalidateTestSuite)) $ condTestSuites gpd
    benchmarks = map (fmap (mapTreeData unvalidateBenchmark)) $ condBenchmarks gpd

jsonSourceRepo :: CabalSpecVersion -> SourceRepo -> Json
jsonSourceRepo v repo =
    JsonObject . map (second toJSON) $ jsonFieldGrammar v (sourceRepoFieldGrammar (repoKind repo)) repo

jsonCustomSetup :: CabalSpecVersion -> SetupBuildInfo -> Json
jsonCustomSetup v = JsonObject . map (second toJSON) . jsonFieldGrammar v (setupBInfoFieldGrammar False)

jsonFlag :: CabalSpecVersion -> PackageFlag -> Json
jsonFlag v flag = JsonObject . map (second toJSON) $ jsonFieldGrammar v (flagFieldGrammar (flagName flag)) flag

type FieldMap a = MonoidalMap String a

jsonCondTree
    :: CabalSpecVersion
    -> JSONFieldGrammar s s
    -> CondTree ConfVar c s
    -> FieldMap [Fragment CondJson]
jsonCondTree ver fg =
    go Nothing . mapTreeData (MonoidalMap . jsonFieldGrammar ver fg)
  where
    go :: Maybe (Condition ConfVar) -> CondTree ConfVar c (FieldMap JsonFragment) -> FieldMap [Fragment CondJson]
    go c (CondNode it _ ifs) =
        fmap (traverse $ \j -> [CondJson c j]) it <> foldMap (goBranch c) ifs

    goBranch :: Maybe (Condition ConfVar) -> CondBranch ConfVar c (FieldMap JsonFragment) -> FieldMap [Fragment CondJson]
    goBranch mc (CondBranch c thenTree elseTree) =
        go (Just (c' `cAnd` c)) thenTree <> foldMap (go (Just (c' `cAnd` cNot c))) elseTree
      where
        c' = fromMaybe (Lit True) mc

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

data CondJson = CondJson (Maybe (Condition ConfVar)) Json
    deriving Show

instance ToJSON CondJson where
    toJSON (CondJson Nothing v) = v
    toJSON (CondJson (Just c) v) = JsonObject ["_if" .= toJSON c, "_then" .= v]
