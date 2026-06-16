{-# LANGUAGE DerivingVia #-}

-- | The @--debug@ pretty-printer: rendering our types back into a cabal-like field
-- and section layout, useful for eyeballing an intermediate stage.
module Cabal.Syntax.Pretty
    ( PrettyFieldClass (..)
    , prettySection
    , ppCondition
    , Vertically (..)
    ) where

import Distribution.Compat.Newtype (Newtype)
import Distribution.Fields.Pretty (PrettyField (..))
import Distribution.Pretty (Pretty (..))
import Distribution.Simple.Utils (toUTF8BS)
import Distribution.Types.CondTree (CondBranch (..), CondTree (..))
import Distribution.Types.Condition (Condition (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Flag (FlagName, unFlagName)

import Text.PrettyPrint (Doc, char, hsep, parens, text, ($$), (<+>))

import Cabal.Syntax.ListMap (ListMap)
import Cabal.Syntax.Utils (FoldableWithIndex (..))

-- | Types that can be rendered as a list of cabal 'PrettyField's — the building
-- blocks of the @--debug@ output.
class PrettyFieldClass a where
    -- | Render a value as zero or more pretty fields\/sections.
    prettyField :: a -> [PrettyField ()]

instance PrettyFieldClass a => PrettyFieldClass (CondTree ConfVar c a) where
    prettyField (CondNode a _ bs) = prettyField a <> concatMap prettyField bs

instance PrettyFieldClass a => PrettyFieldClass (CondBranch ConfVar c a) where
    prettyField (CondBranch c thenTree Nothing) =
        [ prettySection "if" [ppCondition c] (prettyField thenTree)
        ]
    prettyField (CondBranch c thenTree (Just elseTree)) =
        [ prettySection "if" [ppCondition c] (prettyField thenTree)
        , prettySection "else" [] (prettyField elseTree)
        ]

instance Pretty a => PrettyFieldClass (ListMap String a) where
    prettyField m =
        [ PrettyField () (toUTF8BS n) (pretty a)
        | (n, a) <- itoList m
        ]

-- | Build a named cabal section (e.g. an @if@ or @library@ block) from a name,
-- its arguments, and its sub-fields.
prettySection :: String -> [Doc] -> [PrettyField ()] -> PrettyField ()
prettySection n args = PrettySection () (toUTF8BS n) (map pretty args)

-- | Render a 'Condition' in cabal's @if@-expression syntax (e.g. @flag(foo) && !os(windows)@).
ppCondition :: Condition ConfVar -> Doc
ppCondition =
    foldCondition
        (\a -> ppConfVar a)
        (\b -> text (show b))
        (\c -> char '!' <> c)
        (\c1 c2 -> parens (hsep [c1, text "||" <+> c2]))
        (\c1 c2 -> parens (hsep [c1, text "&&" <+> c2]))

foldCondition
    :: (v -> a)
    -> (Bool -> a)
    -> (a -> a)
    -> (a -> a -> a)
    -> (a -> a -> a)
    -> Condition v
    -> a
foldCondition v l n o a = go
  where
    go (Var x) = v x
    go (Lit b) = l b
    go (CNot c) = n (go c)
    go (COr c1 c2) = o (go c1) (go c2)
    go (CAnd c1 c2) = a (go c1) (go c2)

ppConfVar :: ConfVar -> Doc
ppConfVar (OS os) = text "os" <> parens (pretty os)
ppConfVar (Arch arch) = text "arch" <> parens (pretty arch)
ppConfVar (PackageFlag name) = text "flag" <> parens (ppFlagName name)
ppConfVar (Impl c v) = text "impl" <> parens (pretty c <+> pretty v)

ppFlagName :: FlagName -> Doc
ppFlagName = text . unFlagName

-- | A 'Doc' wrapper whose 'Semigroup'\/'Monoid' stacks documents vertically (via
-- '$$') rather than side by side — handy for folding a list of values one per line.
newtype Vertically = Vertically Doc

deriving via Doc instance Pretty Vertically

instance Newtype Doc Vertically

instance Semigroup Vertically where
    Vertically lhs <> Vertically rhs = Vertically (lhs $$ rhs)

instance Monoid Vertically where
    mempty = Vertically mempty
