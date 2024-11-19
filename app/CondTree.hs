{-# LANGUAGE RecordWildCards #-}

module CondTree
    ( -- ** Simplification
      simplifyGenericPackageDescription
    , simplifyCondTree
    , applyEnv
    , Env (..)

      -- ** Transformation
    , pushConditionals
    , flattenCondTree
    , defragC
    , Cond (..)
    , foldCondTree
    , ppCondTree2
    , condTreeJson
    ) where

import Data.Either (partitionEithers)
import Data.Foldable1 (Foldable1 (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE

import Data.Semialign (Align (..), Semialign (..))
import Data.These (these)

import Distribution.Compiler (CompilerId (..))
import Distribution.PackageDescription (cNot)
import Distribution.System (Arch, OS)
import Distribution.Types.CondTree (CondBranch (..), CondTree (..), condIfThen, condIfThenElse)
import Distribution.Types.Condition (Condition (..), cAnd, simplifyCondition)
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Flag (FlagAssignment, PackageFlag (..), lookupFlagAssignment, mkFlagName)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Version (nullVersion)
import Distribution.Types.VersionRange (withinRange)
import Distribution.Utils.Json (Json (..), (.=))

import Data.Maybe (isJust)
import Debug.Trace (traceShow, trace)
import Distribution.Fields.Pretty (PrettyField)
import Distribution.Pretty (Pretty)
import Distribution.Types.Dependency (Dependency)
import FieldMap (FieldMap, ppFieldMap)
import FieldMap qualified
import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..))
import Pretty (ppCondition, prettySection)

simplifyGenericPackageDescription
    :: Env
    -> GenericPackageDescription
    -> GenericPackageDescription
simplifyGenericPackageDescription env (GenericPackageDescription{..}) =
    GenericPackageDescription
        packageDescription
        gpdScannedVersion
        (filterFlags env genPackageFlags)
        (fmap (simplifyCondTree eval) condLibrary)
        ((fmap . fmap) (simplifyCondTree eval) condSubLibraries)
        ((fmap . fmap) (simplifyCondTree eval) condForeignLibs)
        ((fmap . fmap) (simplifyCondTree eval) condExecutables)
        ((fmap . fmap) (simplifyCondTree eval) condTestSuites)
        ((fmap . fmap) (simplifyCondTree eval) condBenchmarks)
  where
    eval :: ConfVar -> Either ConfVar Bool
    eval = applyEnv env

-- | Simplifies a CondTree using a partial flag assignment. Conditions that cannot be evaluated are left untouched.
simplifyCondTree
    :: forall v c a
     . (Monoid a, Monoid c)
    => (v -> Either v Bool)
    -> CondTree v c a
    -> CondTree v c a
simplifyCondTree eval (CondNode a c ifs) =
    CondNode a c branches <> mconcat trees
  where
    (trees, branches) = partitionEithers $ map (simplifyCondBranch eval) ifs

-- | Simplify a CondBranch using a partial variable assignment. Conditions that cannot be evaluated
-- are left unchanged. When we simplify a CondBranch the condition might become always true,
-- transforming the CondBranch into a CondTree. Therefore this function returns either a CondTree or
-- a CondBranch.
simplifyCondBranch
    :: (Monoid a, Monoid c)
    => (v -> Either v Bool)
    -> CondBranch v c a
    -> Either (CondTree v c a) (CondBranch v c a)
simplifyCondBranch eval (CondBranch cv t me) =
    case fst (simplifyCondition cv eval) of
        (Lit True) -> Left $ simplifyCondTree eval t
        (Lit False) -> Left $ maybe mempty (simplifyCondTree eval) me
        cv' -> Right $ CondBranch cv' (simplifyCondTree eval t) (fmap (simplifyCondTree eval) me)

-- * Filter out the flags defined in the environment
filterFlags :: Env -> [PackageFlag] -> [PackageFlag]
filterFlags Env{envFlags} = filter (\f -> isJust $ lookupFlagAssignment (flagName f) envFlags)

data Env = Env
    { envOS :: Maybe OS
    , envArch :: Maybe Arch
    , envCompiler :: Maybe CompilerId
    , envFlags :: FlagAssignment
    }

applyEnv
    :: Env
    -> ConfVar
    -> Either ConfVar Bool
applyEnv Env{envOS = Just os} (OS os') =
    Right (os == os')
applyEnv Env{envArch = Just arch} (Arch arch') =
    Right (arch == arch')
applyEnv Env{envCompiler = Just (CompilerId comp ver)} (Impl comp' ver') =
    Right (ver /= nullVersion && ver `withinRange` ver' && comp == comp')
applyEnv Env{envFlags} (PackageFlag fn) =
    case lookupFlagAssignment fn envFlags of
        Nothing -> Left (PackageFlag fn)
        Just b -> Right b
applyEnv _ var = Left var

test :: CondTree ConfVar () (FieldMap.FieldMap (NonEmpty Bool))
test =
    CondNode
        FieldMap.empty
        mempty
        [ CondBranch
            (Var (PackageFlag (mkFlagName "f")))
            ( CondNode
                (FieldMap.singleton "buildable" (NE.singleton False))
                mempty
                mempty
            )
            Nothing
        ]

pushConditionals
    :: forall f v c a
     . (Align f, Semigroup a, Semigroup c, Show c, Show v, Show (f a), Show (f (CondBranch v c a)), Show (f (CondTree v c a)))
    => CondTree v c (f a)
    -> f (CondTree v c a)
pushConditionals = go
  where
    go :: CondTree v c (f a) -> f (CondTree v c a)
    go n@(CondNode a d ifs) = 
        let r = case NE.nonEmpty ifs of
                    Nothing ->
                        fmap (\a' -> CondNode a' d []) a
                    Just ne ->
                        alignWith
                            ( these
                                (\a' -> CondNode a' d mempty)
                                (\ifs' -> reduce ifs')
                                (\a' ifs' -> CondNode a' d (NE.toList ifs'))
                            )
                            a
                            (crosswalk1 goBranch ne)
        in trace ("\n\ngo\n" ++ show n ++ "\n=\n" ++ show r) r

    goBranch :: CondBranch v c (f a) -> f (CondBranch v c a)
    goBranch b@(CondBranch c thenTree mElseTree) =
        let r =
                case mElseTree of
                    Nothing ->
                        fmap (condIfThen c) $ go thenTree
                    Just elseTree ->
                        alignWith
                            ( these
                                (condIfThen c)
                                (condIfThen (cNot c))
                                (condIfThenElse c)
                            )
                            (go thenTree)
                            (go elseTree)
        in trace ("\n\ngoBranch\n" ++ show b ++ "\n=\n" ++ show r) r

reduce :: (Semigroup a, Semigroup c, Foldable1 f) => f (CondBranch v c a) -> CondTree v c a
reduce = foldMap1 $ \case
    (CondBranch cond (CondNode a c ifs) Nothing) ->
        CondNode a c (map (meetCondition cond) ifs)
    (CondBranch cond (CondNode a c ifs) (Just (CondNode a' c' ifs'))) ->
        CondNode a c (map (meetCondition cond) ifs)
            <> CondNode a' c' (map (meetCondition (cNot cond)) ifs')
  where
    meetCondition c (CondBranch c' t mf) = CondBranch (c `cAnd` c') t mf

foldCondTree
    :: (a -> ([s] -> s))
    -> (Condition v -> s -> s)
    -> (Condition v -> s -> s -> s)
    -> CondTree v c a
    -> s
foldCondTree node ifThen ifThenElse = goNode
  where
    goNode (CondNode it _ ifs) =
        node it (map goBranch ifs)
    goBranch (CondBranch c thenTree Nothing) =
        ifThen c (goNode thenTree)
    goBranch (CondBranch c thenTree (Just elseTree)) =
        ifThenElse c (goNode thenTree) (goNode elseTree)

flattenCondTree :: CondTree v c a -> Cond v a
flattenCondTree (CondNode a _ ifs) =
    Cond a (foldMap (goBranch (Lit True)) ifs)
  where
    go c (CondNode a' _ ifs') =
        (c, a') : foldMap (goBranch c) ifs'
    goBranch c (CondBranch c' thenTree Nothing) =
        go (c `cAnd` c') thenTree
    goBranch c (CondBranch c' thenTree (Just elseTree)) =
        go (c `cAnd` c') thenTree <> go (c `cAnd` cNot c') elseTree

data Cond v a = Cond a [(Condition v, a)]
    deriving (Show, Functor, Foldable, Traversable)

instance (ToJSON a, ToJSON v) => ToJSON (Cond v a) where
    toJSON (Cond a cds) = JsonObject ["always" .= toJSON a, "conditions" .= JsonArray (map jsonCond cds)]

instance Foldable1 (Cond v) where
    foldMap1 :: Semigroup m => (a -> m) -> Cond v a -> m
    foldMap1 f (Cond a cs) = foldMap1 f $ a :| map snd cs

jsonCond :: (ToJSON a, ToJSON b) => (a, b) -> Json
jsonCond (a, b) = JsonObject ["_if" .= toJSON a, "_then" .= toJSON b]

defragC :: Cond ConfVar (Fragment Json) -> Fragment Json
defragC (Cond (ScalarFragment a) cs) =
    case NE.nonEmpty cs of
        Nothing -> ScalarFragment a
        Just cs' -> ListLikeFragment (a `NE.cons` NE.map jsonCond cs')
defragC (Cond (ListLikeFragment as) cs) =
    case NE.nonEmpty cs of
        Nothing -> ListLikeFragment as
        Just cs' -> ListLikeFragment (as <> NE.map jsonCond cs')

class (Functor t, Foldable1 t) => Crosswalk1 t where
    crosswalk1 :: Semialign f => (a -> f b) -> t a -> f (t b)
    crosswalk1 f = sequenceL1 . fmap f

    sequenceL1 :: Semialign f => t (f a) -> f (t a)
    sequenceL1 = crosswalk1 id

    {-# MINIMAL crosswalk1 | sequenceL1 #-}

instance Crosswalk1 NonEmpty where
    crosswalk1 :: Semialign f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
    crosswalk1 f = foldrMap1 (fmap NE.singleton . f) (alignWith cons . f)
      where
        cons = these NE.singleton id NE.cons

condTreeJson :: ToJSON a => CondTree ConfVar c a -> Json
condTreeJson =
    foldCondTree
        ( \a ifs ->
            JsonArray $ toJSON a : ifs
        )
        ( \c thenTree ->
            JsonObject
                [ "if" .= JsonString (show (ppCondition c))
                , "then" .= thenTree
                ]
        )
        ( \c thenTree elseTree ->
            JsonObject
                [ "if" .= JsonString (show (ppCondition c))
                , "then" .= thenTree
                , "else" .= elseTree
                ]
        )

ppCondTree2
    :: (Show a, Pretty a)
    => CondTree ConfVar [Dependency] (FieldMap a)
    -> [PrettyField ()]
ppCondTree2 = go
  where
    go (CondNode it _ ifs) = ppFieldMap it ++ concatMap ppIf ifs

    ppIf (CondBranch c thenTree Nothing) =
        [ prettySection "if" [ppCondition c] (go thenTree)
        ]
    ppIf (CondBranch c thenTree (Just elseTree)) =
        [ prettySection "if" [ppCondition c] (go thenTree)
        , prettySection "else" [] (go elseTree)
        ]
