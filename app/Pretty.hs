module Pretty where
import Text.PrettyPrint (Doc, text, char, parens, hsep, (<+>))
import Distribution.Fields.Pretty (PrettyField (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Condition (Condition (..))
import Distribution.Types.Flag (FlagName, unFlagName)
import Distribution.Pretty (Pretty (..))
import Distribution.Simple.Utils (toUTF8BS)

prettyField :: String -> Doc -> PrettyField ()
prettyField n = PrettyField () (toUTF8BS n)

prettySection :: String -> [Doc] -> [PrettyField ()] -> PrettyField ()
prettySection n args = PrettySection () (toUTF8BS n) (map pretty args)


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
foldCondition var lit not or and = go
  where
    go (Var a) = var a
    go (Lit b) = lit b
    go (CNot c) = not (go c)
    go (COr c1 c2) = or (go c1) (go c2)
    go (CAnd c1 c2) = and (go c1) (go c2)

ppConfVar :: ConfVar -> Doc
ppConfVar (OS os) = text "os" <> parens (pretty os)
ppConfVar (Arch arch) = text "arch" <> parens (pretty arch)
ppConfVar (PackageFlag name) = text "flag" <> parens (ppFlagName name)
ppConfVar (Impl c v) = text "impl" <> parens (pretty c <+> pretty v)

ppFlagName :: FlagName -> Doc
ppFlagName = text . unFlagName
