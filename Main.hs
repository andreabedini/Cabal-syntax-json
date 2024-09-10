{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Data.ByteString.Lazy qualified as BL
import Data.List (foldl1')
import Distribution.CabalSpecVersion
import Distribution.Compat.Lens hiding ((.=))
import Distribution.Compat.Newtype
import Distribution.FieldGrammar
import Distribution.Fields
import Distribution.PackageDescription
import Distribution.PackageDescription.FieldGrammar
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Utils.Generic (fromUTF8BS)
import Distribution.Utils.Json
import Distribution.Utils.ShortText qualified as ST
import Distribution.Verbosity (normal)
import System.Environment (getArgs)
import ToJSON

main :: IO ()
main = do
  fn : _ <- getArgs
  dofile fn

dofile :: FilePath -> IO ()
dofile fn = do
  gpd <- readGenericPackageDescription normal fn
  BL.putStr $ renderJson $ jsonGenericPackageDescription gpd

newtype JSONFieldGrammar s a = JsonFG
  { fieldGrammarJSON :: CabalSpecVersion -> [Condition ConfVar] -> s -> [Pair]
  }
  deriving (Functor)

type JSONFieldGrammar' s = JSONFieldGrammar s s

jsonFieldGrammar :: CabalSpecVersion -> [Condition ConfVar] -> JSONFieldGrammar s a -> s -> [Pair]
jsonFieldGrammar v cs fg = fieldGrammarJSON fg v cs

instance Applicative (JSONFieldGrammar s) where
  pure _ = JsonFG (\_ _ _ -> mempty)
  JsonFG f <*> JsonFG x = JsonFG (\v cs s -> f v cs s <> x v cs s)

instance FieldGrammar ToJSON JSONFieldGrammar where
  blurFieldGrammar :: ALens' a b -> JSONFieldGrammar b d -> JSONFieldGrammar a d
  blurFieldGrammar f (JsonFG fg) = JsonFG $ \v cs ->
    fg v cs . aview f

  uniqueFieldAla :: (ToJSON b, Newtype a b) => FieldName -> (a -> b) -> ALens' s a -> JSONFieldGrammar s a
  uniqueFieldAla fn _pack l = JsonFG $ \_v cs ->
    jsonField cs fn . toJSON . pack' _pack . aview l

  booleanFieldDef :: FieldName -> ALens' s Bool -> Bool -> JSONFieldGrammar s Bool
  booleanFieldDef fn l def = JsonFG $ \_v cs s ->
    let b = aview l s
     in if b == def
          then mempty
          else jsonField cs fn (toJSON b)

  optionalFieldAla :: (ToJSON b, Newtype a b) => FieldName -> (a -> b) -> ALens' s (Maybe a) -> JSONFieldGrammar s (Maybe a)
  optionalFieldAla fn _pack l = JsonFG $ \_ cs s ->
    case aview l s of
      Nothing -> mempty
      Just a -> jsonField cs fn (toJSON (pack' _pack a))

  optionalFieldDefAla :: (ToJSON b, Newtype a b, Eq a) => FieldName -> (a -> b) -> ALens' s a -> a -> JSONFieldGrammar s a
  optionalFieldDefAla fn _pack l def = JsonFG $ \_ cs s ->
    let x = aview l s
     in if x == def
          then mempty
          else jsonField cs fn (toJSON (pack' _pack x))

  freeTextField :: FieldName -> ALens' s (Maybe String) -> JSONFieldGrammar s (Maybe String)
  freeTextField fn l = JsonFG $ \_v cs s ->
    maybe mempty (jsonField cs fn . toJSON) (aview l s)

  freeTextFieldDef :: FieldName -> ALens' s String -> JSONFieldGrammar s String
  freeTextFieldDef fn l = JsonFG $ \_v cs ->
    jsonField cs fn . toJSON . aview l

  freeTextFieldDefST :: FieldName -> ALens' s ST.ShortText -> JSONFieldGrammar s ST.ShortText
  freeTextFieldDefST = defaultFreeTextFieldDefST

  monoidalFieldAla :: (ToJSON b, Monoid a, Newtype a b) => FieldName -> (a -> b) -> ALens' s a -> JSONFieldGrammar s a
  monoidalFieldAla fn _pack l = JsonFG $ \_v cs ->
    jsonField cs fn . toJSON . pack' _pack . aview l

  prefixedFields :: FieldName -> ALens' s [(String, String)] -> JSONFieldGrammar s [(String, String)]
  prefixedFields _fnPfx l = JsonFG $ \_v _cs s ->
    [n .= JsonString v | (n, v) <- aview l s]

  knownField :: FieldName -> JSONFieldGrammar s ()
  knownField _ = pure ()

  deprecatedSince :: CabalSpecVersion -> String -> JSONFieldGrammar s a -> JSONFieldGrammar s a
  deprecatedSince _ _ x = x

  -- TODO: as PrettyFieldGrammar isn't aware of cabal-version: we output the field
  -- this doesn't affect roundtrip as `removedIn` fields cannot be parsed
  -- so invalid documents can be only manually constructed.
  removedIn _ _ x = x

  availableSince _ _ = id

  hiddenField _ = JsonFG (const mempty)

jsonField :: [Condition ConfVar] -> FieldName -> Json -> [Pair]
jsonField _cs _fn (JsonArray []) = mempty
jsonField _cs _fn (JsonObject []) = mempty
jsonField _cs _fn (JsonString []) = mempty
jsonField [] fn v = [fromUTF8BS fn .= v]
jsonField cs fn v = [fromUTF8BS fn .= v']
  where
    v' = JsonObject ["if" .= toJSON (foldl1' CAnd cs), "then" .= v]

jsonGenericPackageDescription :: GenericPackageDescription -> Json
jsonGenericPackageDescription gpd =
  JsonObject $
    concat
      [ jsonPackageDescription v (packageDescription gpd),
        jsonSetupBInfo v (setupBuildInfo (packageDescription gpd)),
        jsonGenPackageFlags v (genPackageFlags gpd),
        jsonCondLibrary v (condLibrary gpd),
        jsonCondSubLibraries v (condSubLibraries gpd),
        jsonCondForeignLibs v (condForeignLibs gpd),
        jsonCondExecutables v (condExecutables gpd),
        jsonCondTestSuites v (condTestSuites gpd),
        jsonCondBenchmarks v (condBenchmarks gpd)
      ]
  where
    v = specVersion $ packageDescription gpd

jsonPackageDescription :: CabalSpecVersion -> PackageDescription -> [Pair]
jsonPackageDescription v pd =
  jsonFieldGrammar v [] packageDescriptionFieldGrammar pd
    <> jsonSourceRepos v (sourceRepos pd)

jsonSourceRepos :: CabalSpecVersion -> [SourceRepo] -> [Pair]
jsonSourceRepos v srs =
  ["source-repos" .= JsonObject (foldMap (jsonSourceRepo v) srs) | not (null srs)]

jsonSourceRepo :: CabalSpecVersion -> SourceRepo -> [Pair]
jsonSourceRepo v repo =
  jsonFieldGrammar v [] (sourceRepoFieldGrammar (repoKind repo)) repo

jsonSetupBInfo :: CabalSpecVersion -> Maybe SetupBuildInfo -> [Pair]
jsonSetupBInfo _ Nothing = mempty
jsonSetupBInfo v (Just sbi) =
  -- TODO: make this configurable
  -- \| defaultSetupDepends sbi = mempty
  ["custom-setup" .= JsonObject vs | not (null vs)]
  where
    vs = jsonFieldGrammar v [] (setupBInfoFieldGrammar False) sbi

jsonGenPackageFlags :: CabalSpecVersion -> [PackageFlag] -> [Pair]
jsonGenPackageFlags v flags = ["flags" .= flags' | not (null flags)]
  where
    flags' =
      JsonObject
        [ unFlagName name .= JsonObject (jsonFieldGrammar v [] (flagFieldGrammar name) flag)
          | flag@(MkPackageFlag name _ _ _) <- flags
        ]

jsonCondLibrary :: CabalSpecVersion -> Maybe (CondTree ConfVar [Dependency] Library) -> [Pair]
jsonCondLibrary _ Nothing = mempty
jsonCondLibrary v (Just condTree) = ["library" .= mainlibJson]
  where
    mainlibJson = jsonCondTree2 v (libraryFieldGrammar LMainLibName) condTree

jsonCondSubLibraries :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Library)] -> [Pair]
jsonCondSubLibraries v libs = ["sub-libraries" .= sublibsJson | not (null libs)]
  where
    sublibsJson =
      JsonObject
        [ unUnqualComponentName n .= jsonCondTree2 v (libraryFieldGrammar $ LSubLibName n) condTree
          | (n, condTree) <- libs
        ]

jsonCondForeignLibs :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)] -> [Pair]
jsonCondForeignLibs v flibs = ["foreign-libraries" .= flibsJson | not (null flibs)]
  where
    flibsJson =
      JsonObject
        [ unUnqualComponentName n .= jsonCondTree2 v (foreignLibFieldGrammar n) condTree | (n, condTree) <- flibs
        ]

jsonCondExecutables :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)] -> [Pair]
jsonCondExecutables v exes = ["executables" .= exesJson | not (null exes)]
  where
    exesJson =
      JsonObject
        [ unUnqualComponentName n .= jsonCondTree2 v (executableFieldGrammar n) condTree
          | (n, condTree) <- exes
        ]

jsonCondTestSuites :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)] -> [Pair]
jsonCondTestSuites v suites = ["test-suites" .= suitesJson | not (null suites)]
  where
    suitesJson =
      JsonObject
        [ unUnqualComponentName n .= jsonCondTree2 v testSuiteFieldGrammar (fmap unvalidateTestSuite condTree)
          | (n, condTree) <- suites
        ]

jsonCondBenchmarks :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)] -> [Pair]
jsonCondBenchmarks v suites = ["benchmarks" .= suitesJson | not (null suites)]
  where
    suitesJson =
      JsonObject
        [ unUnqualComponentName n .= jsonCondTree2 v benchmarkFieldGrammar (fmap unvalidateBenchmark condTree)
          | (n, condTree) <- suites
        ]

jsonCondTree2 :: CabalSpecVersion -> JSONFieldGrammar' s -> CondTree ConfVar [Dependency] s -> Json
jsonCondTree2 v grammar = merge . go []
  where
    go cs (CondNode it _ ifs) =
      jsonFieldGrammar v cs grammar it ++ concatMap (jsonIf cs) ifs

    jsonIf cs (CondBranch c thenTree Nothing) =
      go (c : cs) thenTree
    jsonIf cs (CondBranch c thenTree (Just elseTree)) =
      go (c : cs) thenTree ++ go (CNot c : cs) elseTree

    merge :: [Pair] -> Json
    merge = JsonObject
