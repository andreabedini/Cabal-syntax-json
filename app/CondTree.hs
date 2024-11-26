{-# LANGUAGE RecordWildCards #-}

module CondTree
    ( -- ** Simplification
      simplifyGenericPackageDescription
    , simplifyCondTree
    , applyEnv
    , Env (..)

      -- * Better CondTree
    , MyCondTree (..)
    , MyCondBranch (..)
    , MyCondTree' (..)
    , MyCondBranch' (..)
    , These (..)

      -- ** Transformation
    , banner
    -- , pushConditionals
    , pushConditionals'
    , flattenCondTree
    , defragC
    , Cond (..)
    , foldCondTree
    , condTreeJson
    , convertCondTree
    , reduceOld
    , pushConditionalsOld
    , test
    , convertCondTree'
    ) where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Either (partitionEithers)
import Data.Foldable1 (Foldable1 (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Debug.Trace (trace)

import Data.Semialign (Align (..), Semialign (..))
import Data.These (These (..), these)

import Distribution.Compiler (CompilerId (..))
import Distribution.PackageDescription (cNot)
import Distribution.Pretty (Pretty (..))
import Distribution.System (Arch, OS)
import Distribution.Types.CondTree (CondBranch (..), CondTree (..), condIfThen, condIfThenElse)
import Distribution.Types.Condition (Condition (..), cAnd, simplifyCondition)
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Flag (FlagAssignment, PackageFlag (..), lookupFlagAssignment, mkFlagName)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Version (nullVersion)
import Distribution.Types.VersionRange (withinRange)
import Distribution.Utils.Json (Json (..), (.=))

import Text.PrettyPrint hiding ((<>))

import Data.These.Combinators (justHere, justThere)
import Distribution.Fields.Pretty (CommentPosition (..), showFields)
import FieldMap (FieldMap)
import FieldMap qualified
import Json (ToJSON (..))
import JsonFieldGrammar (Fragment (..))
import Pretty (PrettyFieldClass (..), ppCondition, prettySection)

data MyCondTree v a = MyCondNode
    { myCondTreeData :: a
    , myCondTreeComponents :: [MyCondBranch v a]
    }
    deriving (Show, Functor, Foldable, Traversable)

instance PrettyFieldClass a => Pretty (MyCondTree ConfVar a) where
    pretty = text . showFields (const NoComment) . prettyField

data MyCondBranch v a = MyCondBranch
    { myCondBranchCondition :: Condition v
    , myCondBranchOptions :: These (MyCondTree v a) (MyCondTree v a)
    }
    deriving Show

instance Functor (MyCondBranch v) where
    fmap f (MyCondBranch c a) = MyCondBranch c (bimap (fmap f) (fmap f) a)

instance Foldable (MyCondBranch v) where
    foldMap f (MyCondBranch _ a) = bifoldMap (foldMap f) (foldMap f) a

instance Traversable (MyCondBranch v) where
    traverse f (MyCondBranch c a) = MyCondBranch c <$> bitraverse (traverse f) (traverse f) a

instance PrettyFieldClass a => Pretty (MyCondBranch ConfVar a) where
    pretty = text . showFields (const NoComment) . prettyField

instance PrettyFieldClass a => PrettyFieldClass (MyCondTree ConfVar a) where
    prettyField (MyCondNode it ifs) = prettyField it ++ concatMap prettyField ifs

instance PrettyFieldClass a => PrettyFieldClass (MyCondBranch ConfVar a) where
    prettyField (MyCondBranch c t) =
        [ prettySection "if" [ppCondition c] $
            [ prettySection "then" [] (foldMap prettyField (justHere t))
            , prettySection "else" [] (foldMap prettyField (justThere t))
            ]
        ]

convertCondTree :: CondTree v c a -> MyCondTree v a
convertCondTree = go
  where
    go (CondNode a _ ifs) = MyCondNode a (map goBranch ifs)
    goBranch (CondBranch c thenTree Nothing) = MyCondBranch c (This (go thenTree))
    goBranch (CondBranch c thenTree (Just elseTree)) = MyCondBranch c (These (go thenTree) (go elseTree))

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

instance Semigroup a => Semigroup (MyCondTree v a) where
    MyCondNode lhs lbranches <> MyCondNode rhs rbranches =
        MyCondNode (lhs <> rhs) (lbranches <> rbranches)

pushConditionalsOld
    :: forall f v c a
     . ( Align f
       , Show v
       , Show (f a)
       , Show a
       , Show (f (CondTree v c a))
       , Semigroup a
       , Semigroup c
       , Show c
       , Show (f (CondBranch v c a))
       )
    => CondTree v c (f a)
    -> f (CondTree v c a)
pushConditionalsOld = go
  where
    go :: CondTree v c (f a) -> f (CondTree v c a)
    go =
        traceF (banner "go") $ \(CondNode a d ifs) ->
            case NE.nonEmpty ifs of
                Nothing ->
                    fmap (\a' -> CondNode a' d []) a
                -- WRONG: BAD:
                -- NOTE: I cannot mutate the structure while I align!
                -- ERROR:
                Just ne ->
                    alignWith
                        ( these
                            (\a' -> CondNode a' d mempty)
                            (\ifs' -> reduceOld ifs')
                            (\a' ifs' -> CondNode a' d (NE.toList ifs'))
                        )
                        a
                        (crosswalk1 goBranch ne)

    goBranch :: CondBranch v c (f a) -> f (CondBranch v c a)
    goBranch = traceF (banner "goBranch") $ \(CondBranch c thenTree mElseTree) ->
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

banner :: [Char] -> String
banner name =
    unlines
        ["", replicate (length name + 8) '-', unwords ["---", name, "---"], replicate (length name + 8) '-']

-- WARN This function is wrong.
reduceOld
    :: (Semigroup a, Semigroup c, Foldable1 f, Show a, Show c, Show v, Show (f (CondBranch v c a)))
    => f (CondBranch v c a)
    -> CondTree v c a
reduceOld =
    traceF
        (banner "reduceOld")
        $ foldMap1
        $ \case
            --
            (CondBranch cond (CondNode a c ifs) Nothing) ->
                CondNode a c (map (meetCondition cond) ifs)
            --
            (CondBranch cond (CondNode a c ifs) (Just (CondNode a' c' ifs'))) ->
                CondNode a c (map (meetCondition cond) ifs)
                    <> CondNode a' c' (map (meetCondition (cNot cond)) ifs')
  where
    meetCondition c (CondBranch c' t mf) = CondBranch (c `cAnd` c') t mf

traceF :: (Show a1, Show a2) => String -> (a1 -> a2) -> a1 -> a2
traceF name f input =
    let r = f input
     in trace (unlines [name, show input, "=", show r]) r

foldCondTree
    :: (a -> [s] -> s)
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
    toJSON (Cond a cds) =
        JsonObject
            [ "always" .= toJSON a
            , "conditions" .= JsonArray (map jsonCond cds)
            ]

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

data MyCondTree' v a = MyCondNode' (These a (NonEmpty (MyCondBranch' v a)))
    deriving Show

instance Semigroup a => Semigroup (MyCondTree' v a) where
    MyCondNode' tl <> MyCondNode' tr = MyCondNode' (tl <> tr)

instance Functor (MyCondTree' v) where
    fmap f (MyCondNode' t) = MyCondNode' (bimap f (fmap (fmap f)) t)

instance PrettyFieldClass a => PrettyFieldClass (MyCondTree' ConfVar a) where
    prettyField (MyCondNode' t) = bifoldMap prettyField (concatMap prettyField) t

instance Pretty a => Pretty (MyCondTree' ConfVar a) where
    pretty (MyCondNode' t) =
        vcat
            [ maybe empty pretty (justHere t)
            , maybe empty (vcat . map pretty . NE.toList) (justThere t)
            ]

data MyCondBranch' v a = MyCondBranch' (Condition v) (These (MyCondTree' v a) (MyCondTree' v a))
    deriving Show

instance Functor (MyCondBranch' v) where
    fmap f (MyCondBranch' c t) = MyCondBranch' c (bimap (fmap f) (fmap f) t)

instance PrettyFieldClass a => PrettyFieldClass (MyCondBranch' ConfVar a) where
    prettyField (MyCondBranch' c t) =
        [ prettySection "if" [ppCondition c] $
            [ prettySection "then" [] (foldMap prettyField (justHere t))
            , prettySection "else" [] (foldMap prettyField (justThere t))
            ]
        ]

instance Pretty a => Pretty (MyCondBranch' ConfVar a) where
    pretty (MyCondBranch' c t) =
        vcat $
            mconcat
                [ [text "if" <+> ppCondition c]
                , [text "then" $$ nest 2 (pretty h) | Just h <- [justHere t]]
                , [text "else" $$ nest 2 (pretty h) | Just h <- [justThere t]]
                ]

pushConditionals'
    :: forall a v
     . Semigroup a
    => MyCondTree' v (FieldMap a)
    -> FieldMap (MyCondTree' v a)
pushConditionals' (MyCondNode' (This a)) =
    fmap (MyCondNode' . This) a
pushConditionals' (MyCondNode' (That b)) =
    fmap (MyCondNode' . That) (pushConditionals'B b)
pushConditionals' (MyCondNode' (These a b)) =
    let x = fmap (MyCondNode' . This) a
        y = fmap (MyCondNode' . That) (pushConditionals'B b)
     in x <> y

pushConditionals'B
    :: Semigroup a
    => NonEmpty (MyCondBranch' v (FieldMap a))
    -> FieldMap (NonEmpty (MyCondBranch' v a))
pushConditionals'B = foldMap1 $ \(MyCondBranch' c theseTrees) ->
    bifoldMap
        (fmap (NE.singleton . MyCondBranch' c . This) . pushConditionals')
        (fmap (NE.singleton . MyCondBranch' c . That) . pushConditionals')
        theseTrees

-- case theseTrees of
--     (This t) ->
--         fmap (NE.singleton . MyCondBranch' c . This) $ pushConditionals' t
--     (That t) ->
--         fmap (NE.singleton . MyCondBranch' c . That) $ pushConditionals' t
--     (These t f) ->
--         (fmap (NE.singleton . MyCondBranch' c . This) $ pushConditionals' t)
--             <> (fmap (NE.singleton . MyCondBranch' c . That) $ pushConditionals' f)

convertCondTree' :: CondTree v c a -> MyCondTree' v a
convertCondTree' = go
  where
    go (CondNode a _ ifs) = case NE.nonEmpty ifs of
        Nothing -> MyCondNode' (This a)
        Just ne -> MyCondNode' (These a (fmap goBranch ne))

    goBranch (CondBranch c thenTree Nothing) =
        MyCondBranch' c (This (go thenTree))
    goBranch (CondBranch c thenTree (Just elseTree)) =
        MyCondBranch' c (These (go thenTree) (go elseTree))
