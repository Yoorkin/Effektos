module JS.Flat
  ( Name,
    Program (..),
    Fn (..),
    Binding (..),
    Value (..),
    Expr (..),
  )
where

import Prettyprinter
import qualified Syntax.Constant as C
import Syntax.Primitive
import Util.CompileEnv
import Util.Prettyprint

data Program
  = Program Fn [Fn]

data Fn
  = Fn Name [Name] [Binding] Expr
  deriving (Show)

data Binding = Binding Name Value
  deriving (Show)

data Value
  = Unit
  | I32 Int
  | Var Name
  | Proj Int Name
  | Tuple [Name]
  | PrimOp Primitive [Name]
  deriving (Show)

instance Pretty Value where
  pretty Unit = pretty "Unit"
  pretty (I32 v) = pretty v
  pretty (Var n) = pretty n
  pretty (Proj i n) = pretty n <> brackets (pretty i)
  pretty (Tuple ns) = parens (sepMap (comma <> space) pretty ns)
  pretty v@(PrimOp {}) = pretty $ show v

type Effect = String

data Expr
  = Apply Name [Name]
  | Switch Name [C.Constant] [Expr] (Maybe Expr)
  | Handle Expr [(Effect, Name)]
  | Raise Effect [Name]
  | Exit Name
  deriving (Show)

(<//>) :: Doc ann -> Doc ann -> Doc ann
(<//>) a b = a <> hardline <> b

nested :: Doc ann -> Doc ann
nested = nest 4

instance Pretty Program where
  pretty (Program m fns) = concatWith (<//>) (map pretty (m : fns))

instance Pretty Binding where
  pretty (Binding n v) = pretty n <+> pretty "=" <+> pretty v

instance Pretty Fn where
  pretty (Fn n args bindings expr) =
    pretty n
      <> parens (sepMap (comma <> space) pretty args)
      <> pretty ":"
      <> nested (hardline <> concatWith (<>) (map (\x -> pretty x <> hardline) bindings) <> pretty expr)
      <> hardline

instance Pretty Expr where
  pretty (Apply f args) = pretty f <> parens (sepMap (comma <> space) pretty args)
  pretty (Switch cond index branches fallback) =
    pretty "switch"
      <+> pretty cond
      <> colon
      <> nested (hardline <> sepMap hardline f (zip index branches))
      <> case fallback of (Just fb) -> pretty "_ ->" <+> pretty fb; Nothing -> pretty ""
    where
      f (i, e) = pretty (show i) <+> pretty "->" <+> pretty e
  pretty (Exit n) = pretty "exit" <+> pretty n
  pretty (Handle e hdls) = pretty "handle" <> nested (line <> pretty e) <> line <> pretty "with" <+> pretty hdls
  pretty (Raise eff xs) = pretty "raise" <+> pretty eff <+> parens (sepMap comma pretty xs)

instance Show Program where
  show = render . pretty
