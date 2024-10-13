{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Data.ByteString.Lazy qualified as BL
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
  fn_in : mb_fn_out <- getArgs
  gpd <- readGenericPackageDescription normal fn_in
  let bs = renderJson $ jsonGenericPackageDescription gpd
  case mb_fn_out of
    [] -> BL.putStr bs
    fn_out : _ -> BL.writeFile fn_out bs

newtype JSONFieldGrammar s a = JsonFG
  { fieldGrammarJSON :: CabalSpecVersion -> s -> [Pair]
  }
  deriving (Functor)

type JSONFieldGrammar' s = JSONFieldGrammar s s

jsonFieldGrammar :: CabalSpecVersion -> JSONFieldGrammar s a -> s -> [Pair]
jsonFieldGrammar v fg = fieldGrammarJSON fg v

instance Applicative (JSONFieldGrammar s) where
  pure _ = JsonFG (\_ _ -> mempty)
  JsonFG f <*> JsonFG x = JsonFG (\v s -> f v s <> x v s)

instance FieldGrammar ToJSON JSONFieldGrammar where
  blurFieldGrammar :: ALens' a b -> JSONFieldGrammar b d -> JSONFieldGrammar a d
  blurFieldGrammar f (JsonFG fg) = JsonFG $ \v ->
    fg v . aview f

  uniqueFieldAla :: (ToJSON b, Newtype a b) => FieldName -> (a -> b) -> ALens' s a -> JSONFieldGrammar s a
  uniqueFieldAla fn _pack l = JsonFG $ \_v ->
    jsonField fn . toJSON . pack' _pack . aview l

  booleanFieldDef :: FieldName -> ALens' s Bool -> Bool -> JSONFieldGrammar s Bool
  booleanFieldDef fn l def = JsonFG $ \_v s ->
    let b = aview l s
     in if b == def
          then mempty
          else jsonField fn (toJSON b)

  optionalFieldAla :: (ToJSON b, Newtype a b) => FieldName -> (a -> b) -> ALens' s (Maybe a) -> JSONFieldGrammar s (Maybe a)
  optionalFieldAla fn _pack l = JsonFG $ \_ s ->
    case aview l s of
      Nothing -> mempty
      Just a -> jsonField fn (toJSON (pack' _pack a))

  optionalFieldDefAla :: (ToJSON b, Newtype a b, Eq a) => FieldName -> (a -> b) -> ALens' s a -> a -> JSONFieldGrammar s a
  optionalFieldDefAla fn _pack l def = JsonFG $ \_ s ->
    let x = aview l s
     in if x == def
          then mempty
          else jsonField fn (toJSON (pack' _pack x))

  freeTextField :: FieldName -> ALens' s (Maybe String) -> JSONFieldGrammar s (Maybe String)
  freeTextField fn l = JsonFG $ \_v s ->
    maybe mempty (jsonField fn . toJSON) (aview l s)

  freeTextFieldDef :: FieldName -> ALens' s String -> JSONFieldGrammar s String
  freeTextFieldDef fn l = JsonFG $ \_v ->
    jsonField fn . toJSON . aview l

  freeTextFieldDefST :: FieldName -> ALens' s ST.ShortText -> JSONFieldGrammar s ST.ShortText
  freeTextFieldDefST = defaultFreeTextFieldDefST

  monoidalFieldAla :: (ToJSON b, Monoid a, Newtype a b) => FieldName -> (a -> b) -> ALens' s a -> JSONFieldGrammar s a
  monoidalFieldAla fn _pack l = JsonFG $ \_v ->
    jsonField fn . toJSON . pack' _pack . aview l

  prefixedFields :: FieldName -> ALens' s [(String, String)] -> JSONFieldGrammar s [(String, String)]
  prefixedFields _fnPfx l = JsonFG $ \_v s ->
    [n .= JsonString v | (n, v) <- aview l s]

  knownField :: FieldName -> JSONFieldGrammar s ()
  knownField _ = pure ()

  deprecatedSince :: CabalSpecVersion -> String -> JSONFieldGrammar s a -> JSONFieldGrammar s a
  deprecatedSince _ _ x = x

  -- FIXME: I think this comment got magled, check the original
  -- TODO: as PrettyFieldGrammar isn't aware of cabal-version: we output the field
  -- this doesn't affect roundtrip as `removedIn` fields cannot be parsed
  -- so invalid documents can be only manually constructed.
  removedIn _ _ x = x

  availableSince _ _ = id

  hiddenField _ = JsonFG (const mempty)

jsonField :: FieldName -> Json -> [Pair]
jsonField _fn (JsonArray []) = mempty
jsonField _fn (JsonObject []) = mempty
jsonField _fn (JsonString []) = mempty
jsonField fn v = [fromUTF8BS fn .= v]

-- FIXME: too many fmaps
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
        jsonCondTestSuites v (fmap (fmap (mapTreeData unvalidateTestSuite)) $ condTestSuites gpd),
        jsonCondBenchmarks v (fmap (fmap (mapTreeData unvalidateBenchmark)) $ condBenchmarks gpd)
      ]
  where
    v = specVersion $ packageDescription gpd

jsonPackageDescription :: CabalSpecVersion -> PackageDescription -> [Pair]
jsonPackageDescription v pd =
  jsonFieldGrammar v packageDescriptionFieldGrammar pd
    <> jsonSourceRepos v (sourceRepos pd)

jsonSourceRepos :: CabalSpecVersion -> [SourceRepo] -> [Pair]
jsonSourceRepos v srs =
  ["source-repos" .= JsonObject (foldMap (jsonSourceRepo v) srs) | not (null srs)]

jsonSourceRepo :: CabalSpecVersion -> SourceRepo -> [Pair]
jsonSourceRepo v repo =
  jsonFieldGrammar v (sourceRepoFieldGrammar (repoKind repo)) repo

jsonSetupBInfo :: CabalSpecVersion -> Maybe SetupBuildInfo -> [Pair]
jsonSetupBInfo _ Nothing = mempty
jsonSetupBInfo v (Just sbi) =
  -- TODO: make this configurable
  -- \| defaultSetupDepends sbi = mempty
  ["custom-setup" .= JsonObject vs | not (null vs)]
  where
    vs = jsonFieldGrammar v (setupBInfoFieldGrammar False) sbi

jsonGenPackageFlags :: CabalSpecVersion -> [PackageFlag] -> [Pair]
jsonGenPackageFlags v flags = ["flags" .= flags' | not (null flags)]
  where
    flags' =
      JsonObject
        [ unFlagName name .= JsonObject (jsonFieldGrammar v (flagFieldGrammar name) flag)
        | flag@(MkPackageFlag name _ _ _) <- flags
        ]

jsonCondLibrary :: CabalSpecVersion -> Maybe (CondTree ConfVar [Dependency] Library) -> [Pair]
jsonCondLibrary _ Nothing = mempty
jsonCondLibrary v (Just condTree) = ["library" .= JsonObject mainlibJson]
  where
    mainlibJson = jsonCondTree v (libraryFieldGrammar LMainLibName) condTree

-- NOTE: Can we reduce this boilerplate?

jsonCondSubLibraries :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Library)] -> [Pair]
jsonCondSubLibraries v libs = ["sub-libraries" .= sublibsJson | not (null libs)]
  where
    sublibsJson =
      JsonObject
        [ unUnqualComponentName n .= JsonObject (jsonCondTree v (libraryFieldGrammar $ LSubLibName n) condTree)
        | (n, condTree) <- libs
        ]

jsonCondForeignLibs :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)] -> [Pair]
jsonCondForeignLibs v flibs = ["foreign-libraries" .= flibsJson | not (null flibs)]
  where
    flibsJson =
      JsonObject
        [ unUnqualComponentName n .= JsonObject (jsonCondTree v (foreignLibFieldGrammar n) condTree) | (n, condTree) <- flibs
        ]

jsonCondExecutables :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)] -> [Pair]
jsonCondExecutables v exes = ["executables" .= exesJson | not (null exes)]
  where
    exesJson =
      JsonObject
        [ unUnqualComponentName n .= JsonObject (jsonCondTree v (executableFieldGrammar n) condTree)
        | (n, condTree) <- exes
        ]

jsonCondTestSuites :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuiteStanza)] -> [Pair]
jsonCondTestSuites v suites = ["test-suites" .= suitesJson | not (null suites)]
  where
    suitesJson =
      JsonObject
        [ unUnqualComponentName n .= JsonObject (jsonCondTree v testSuiteFieldGrammar condTree)
        | (n, condTree) <- suites
        ]

jsonCondBenchmarks :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] BenchmarkStanza)] -> [Pair]
jsonCondBenchmarks v suites = ["benchmarks" .= suitesJson | not (null suites)]
  where
    suitesJson =
      JsonObject
        [ unUnqualComponentName n .= JsonObject (jsonCondTree v benchmarkFieldGrammar condTree)
        | (n, condTree) <- suites
        ]

jsonCondTree :: CabalSpecVersion -> JSONFieldGrammar' s -> CondTree ConfVar [Dependency] s -> [Pair]
jsonCondTree v fg = goTree
  where
    goTree (CondNode a _c ifs) =
      jsonFieldGrammar v fg a ++ concatMap goBranch ifs
    goBranch (CondBranch c thenTree Nothing) =
      [ "_if" .= toJSON c,
        "_then" .= JsonObject (jsonCondTree v fg thenTree)
      ]
    goBranch (CondBranch c thenTree (Just elseTree)) =
      [ "_if" .= toJSON c,
        "_then" .= JsonObject (jsonCondTree v fg thenTree),
        "_else" .= JsonObject (jsonCondTree v fg elseTree)
      ]

-- jsonIf  (CondBranch c thenTree Nothing) =
--   go  thenTree
-- jsonIf  (CondBranch c thenTree (Just elseTree)) =
--   go  thenTree ++ go (CNot c : cs) elseTree
