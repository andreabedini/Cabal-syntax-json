{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module CondTree
    ( -- ** Simplification
      simplifyGenericPackageDescription
    , simplifyCondTree
    , applyEnv
    , Env (..)

      -- ** Better CondTree
    , MyCondTree (..)
    , MyCondBranch (..)
    , These (..)
    , convertCondTree
    , defragC
    , flattenCondTree
    -- , foldCondTree
    , pushConditionals
    , Cond (..)
    ) where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Either (partitionEithers)
import Data.Foldable1 (Foldable1 (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)

import Distribution.Compat.Newtype
import Distribution.Compiler (CompilerId (..))
import Distribution.PackageDescription (cNot)
import Distribution.Pretty (Pretty (..))
import Distribution.System (Arch, OS)
import Distribution.Types.CondTree (CondBranch (..), CondTree (..))
import Distribution.Types.Condition (Condition (..), cAnd, simplifyCondition)
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Flag (FlagAssignment, PackageFlag (..), lookupFlagAssignment)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Version (nullVersion)
import Distribution.Types.VersionRange (withinRange)
import Distribution.Utils.Json (Json (..), (.=))

import Text.PrettyPrint hiding ((<>))

import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..))
import ListMap (ListMap)
import Pretty (PrettyFieldClass (..), Vertically (..), ppCondition, prettySection)
import These (Semialign (..), These (..), justHere, justThere, these)

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

data CondValue v a = CondValue (Condition v) a
    deriving (Show, Functor, Foldable, Traversable)

instance (ToJSON v, ToJSON a) => ToJSON (CondValue v a) where
    toJSON (CondValue v a) = JsonObject ["_if" .= toJSON v, "_then" .= toJSON a]

data Cond v a = Cond (These a (NonEmpty (CondValue v a)))
    deriving Show

instance Functor (Cond v) where
    fmap f (Cond t) = Cond (bimap f (fmap (fmap f)) t)

instance Pretty a => Pretty (Cond ConfVar a) where
    pretty (Cond t) =
        foldMap pretty (justHere t) $$ foldMap prettyConditions (justThere t)
      where
        prettyConditions =
            alaf Vertically foldMap $
                \(CondValue c v) -> vcat [text "if" <+> ppCondition c <+> text "then", nest 2 (pretty v)]

instance (ToJSON a, ToJSON v) => ToJSON (Cond v a) where
    toJSON (Cond (This a)) =
        toJSON a
    toJSON (Cond (That as)) =
        toJSON as
    toJSON (Cond (These a as)) =
        JsonArray $ toJSON a : map toJSON (NE.toList as)

flattenCondTree :: MyCondTree v a -> Cond v a
flattenCondTree = go0
  where
    go0 (MyCondNode (This t)) =
        Cond (This t)
    go0 (MyCondNode (That bs)) =
        Cond (That (foldMap1 (goBranch (Lit True)) bs))
    go0 (MyCondNode (These t bs)) =
        Cond (These t (foldMap1 (goBranch (Lit True)) bs))

    go :: Condition v -> MyCondTree v a -> NonEmpty (CondValue v a)
    go c (MyCondNode (This t)) =
        NE.singleton (CondValue c t)
    go c (MyCondNode (That bs)) =
        foldMap1 (goBranch c) bs
    go c (MyCondNode (These t bs)) =
        CondValue c t `NE.cons` (foldMap1 (goBranch c) bs)

    goBranch :: Condition v -> MyCondBranch v a -> NonEmpty (CondValue v a)
    goBranch c (MyCondBranch c' thenTree Nothing) =
        go (c `cAnd` c') thenTree
    goBranch c (MyCondBranch c' thenTree (Just elseTree)) =
        go (c `cAnd` c') thenTree <> go (c `cAnd` cNot c') elseTree

defragC :: Cond ConfVar (Fragment Json) -> Json
defragC (Cond (This (ScalarFragment a))) =
    a
defragC (Cond (This (ListLikeFragment as))) =
    JsonArray (NE.toList as)
defragC (Cond (That bs)) =
    toJSON bs
defragC (Cond (These (ScalarFragment a) cs)) =
    JsonArray $ a : map toJSON (NE.toList cs)
defragC (Cond (These (ListLikeFragment bs) cs)) =
    JsonArray $ NE.toList bs <> map toJSON (NE.toList cs)

data MyCondTree v a = MyCondNode (These a (NonEmpty (MyCondBranch v a)))
    deriving Show

instance Semigroup a => Semigroup (MyCondTree v a) where
    MyCondNode tl <> MyCondNode tr = MyCondNode (tl <> tr)

instance Functor (MyCondTree v) where
    fmap f (MyCondNode t) = MyCondNode (bimap f (fmap (fmap f)) t)

instance Foldable (MyCondTree v) where
    foldMap f (MyCondNode theseTrees) = bifoldMap f (foldMap (foldMap f)) theseTrees

instance PrettyFieldClass a => PrettyFieldClass (MyCondTree ConfVar a) where
    prettyField (MyCondNode t) = bifoldMap prettyField (concatMap prettyField) t

instance Pretty a => Pretty (MyCondTree ConfVar a) where
    pretty (MyCondNode t) =
        vcat
            [ maybe empty pretty (justHere t)
            , maybe empty (vcat . map pretty . NE.toList) (justThere t)
            ]

instance ToJSON a => ToJSON (MyCondTree ConfVar a) where
    toJSON (MyCondNode t) =
        these
            toJSON
            (JsonArray . map toJSON . NE.toList)
            (\t' f' -> JsonArray (toJSON t' : map toJSON (NE.toList f')))
            t

data MyCondBranch v a = MyCondBranch (Condition v) (MyCondTree v a) (Maybe (MyCondTree v a))
    deriving (Show, Functor, Foldable)

instance ToJSON a => ToJSON (MyCondBranch ConfVar a) where
    toJSON (MyCondBranch c thenTree Nothing) =
        JsonObject
            [ "if" .= JsonString (show (ppCondition c))
            , "then" .= toJSON thenTree
            ]
    toJSON (MyCondBranch c thenTree (Just elseTree)) =
        JsonObject
            [ "if" .= JsonString (show (ppCondition c))
            , "then" .= toJSON thenTree
            , "else" .= toJSON elseTree
            ]

instance PrettyFieldClass a => PrettyFieldClass (MyCondBranch ConfVar a) where
    prettyField (MyCondBranch c thenTree Nothing) =
        [ prettySection "if" [ppCondition c] $
            [ prettySection "then" [] (foldMap prettyField thenTree)
            ]
        ]
    prettyField (MyCondBranch c thenTree (Just elseTree)) =
        [ prettySection "if" [ppCondition c] $
            [ prettySection "then" [] (foldMap prettyField thenTree)
            , prettySection "else" [] (foldMap prettyField elseTree)
            ]
        ]

instance Pretty a => Pretty (MyCondBranch ConfVar a) where
    pretty (MyCondBranch c thenTree Nothing) =
        vcat $
            [ text "if" <+> ppCondition c
            , text "then" $$ nest 2 (pretty thenTree)
            ]
    pretty (MyCondBranch c thenTree (Just elseTree)) =
        vcat $
            [ text "if" <+> ppCondition c
            , text "then" $$ nest 2 (pretty thenTree)
            , text "else" $$ nest 2 (pretty elseTree)
            ]

pushConditionals
    :: Semigroup a
    => MyCondTree v (ListMap String a)
    -> ListMap String (MyCondTree v a)
pushConditionals (MyCondNode (This a)) =
    fmap (MyCondNode . This) a
pushConditionals (MyCondNode (That b)) =
    fmap (MyCondNode . That) (pushConditionals'B b)
pushConditionals (MyCondNode (These a b)) =
    let x = fmap (MyCondNode . This) a
        y = fmap (MyCondNode . That) (pushConditionals'B b)
     in x <> y

pushConditionals'B
    :: Semigroup a
    => NonEmpty (MyCondBranch v (ListMap String a))
    -> ListMap String (NonEmpty (MyCondBranch v a))
pushConditionals'B = foldMap1 $ \case
    (MyCondBranch c thenTree Nothing) ->
        fmap
            (\t -> NE.singleton $ MyCondBranch c t Nothing)
            (pushConditionals thenTree)
    (MyCondBranch c thenTree (Just elseTree)) ->
        alignWith
            ( these
                (\t -> NE.singleton $ MyCondBranch c t Nothing)
                (\t -> NE.singleton $ MyCondBranch (cNot c) t Nothing)
                (\t f -> NE.singleton $ MyCondBranch c t (Just f))
            )
            (pushConditionals thenTree)
            (pushConditionals elseTree)

convertCondTree :: CondTree v c a -> MyCondTree v a
convertCondTree (CondNode a _ ifs) =
    case NE.nonEmpty ifs of
        Nothing -> MyCondNode (This a)
        Just ne -> MyCondNode (These a (fmap convertCondBranch ne))

convertCondBranch :: CondBranch v c a -> MyCondBranch v a
convertCondBranch (CondBranch c thenTree mElseTree) =
    MyCondBranch c (convertCondTree thenTree) (fmap convertCondTree mElseTree)
