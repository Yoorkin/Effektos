{-# LANGUAGE DeriveDataTypeable #-}

module CPS where

import Control.Lens.Plated
import Data.Data
import Data.Data.Lens
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Primitive
import CompileEnv


data Value
  = Var Name
  | I32 Int
  | Unit
  | Tuple [Name]
  | Cont (Maybe Name) Name Term -- env x e
  | Fn Name (Maybe Name) [Name] Term -- k env x e
  deriving (Eq, Ord, Read, Data)

data Term
  = LetVal Name Value Term
  | LetSel Name Int Name Term -- TODO: Value to String, the Value should be (Var x)
  | LetCont Name (Maybe Name) Name Term Term
  | LetFns [(Name, Value)] Term
  | Continue Name (Maybe Name) Name
  | Apply Name Name (Maybe Name) [Name]
  | LetPrim Name Primitive [Name] Term
  | Switch Name [Int] [Term] (Maybe Term)
  | Handle String Name Term -- h fn
  | Raise Name Name (Maybe Name) [Name] -- h k x
  | Halt Name
  deriving (Eq, Ord, Read, Data)

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty (defaultLayoutOptions {layoutPageWidth = AvailablePerLine 50 1.0})

instance Show Value where
  show = renderDoc . pretty

instance Show Term where
  show = renderDoc . pretty

instance Plated Value where
  plate = uniplate

instance Plated Term where
  plate = uniplate

(</>) :: Doc ann -> Doc ann -> Doc ann
(</>) a b = a <> line <> b

(<//>) :: Doc ann -> Doc ann -> Doc ann
(<//>) a b = a <> hardline <> b

nested :: Doc ann -> Doc ann
nested = nest 2

instance Pretty Value where
  pretty (Var n) = pretty n
  pretty (I32 i) = pretty "i32" <+> pretty i
  pretty Unit = pretty "()"
  pretty (Tuple es) = lparen <> concatWith (\x y -> x <> comma <+> y) (map pretty es) <> rparen
  pretty (Cont env n t) = group (pretty "λ" <> braces (pretty env) <+> pretty n <> dot <> pretty t)
  pretty (Fn k env x t) = group (parens $ pretty "λ" <+> pretty k <+> braces (pretty env) <+> pretty x <+> dot <> nested (line <> pretty t))

instance Pretty Term where
  pretty (LetVal n v t) =
    group
      ( pretty
          "Letval"
          <> nested
            ( line
                <> pretty n
                <+> pretty "="
                <> group (nested (line <> pretty v))
            )
            </> pretty "in"
      )
      </> pretty t
  pretty (LetSel n i n' t) =
    pretty "Letsel"
      <+> pretty n
      <+> group
        ( nested
            ( pretty "="
                </> ( pretty "select"
                        <+> pretty i
                        <+> pretty n'
                    )
            )
            </> pretty "in"
        )
        </> pretty t
  pretty (LetCont k env x c t) =
    group $
      pretty "Letcont"
        <> nested
          ( softline
              <> pretty k
              <+> braces (pretty env)
              <+> pretty x
              <+> pretty "="
              <> nested
                ( line
                    <> pretty c
                )
          )
          </> pretty "in"
          </> pretty t
  pretty (Continue k env x) = group (pretty "Continue" <+> pretty k <+> braces (pretty env) <+> pretty x)
  pretty (Apply f k env x) = group (pretty f <+> pretty k <+> braces (pretty env) <+> pretty x)
  pretty (LetPrim n op ns t) =
    group
      ( pretty
          "Letprim"
          <> nested
            ( line
                <> pretty n
                <+> pretty "="
                <> group
                  ( nested
                      ( line
                          <> pretty (show op)
                          <+> lparen
                          <> concatWith (\x y -> x <> comma <+> y) (map pretty ns)
                          <> rparen
                      )
                  )
            )
            </> pretty "in"
      )
      </> pretty t
  pretty (Halt e) = group (pretty "halt" </> pretty e)
  pretty (Handle h hf l) = group (pretty "Lethandle" <+> pretty h <+> pretty "=" <+> pretty hf <+> pretty "in" </> pretty l)
  pretty (Raise h k env xs) = group (pretty "raise" <+> pretty h <+> pretty k <+> braces (pretty env) <+> pretty xs)
  pretty (LetFns fns l) = group (pretty "Letrec" <> nest 2 (line <> concatWith (</>) (map g fns)) </> pretty "in" </> pretty l)
    where
      g (n, v) = group (pretty n <+> pretty "=" <+> pretty v)
  pretty (Switch n ix bs fb) =
    pretty "switch"
      <+> pretty n
      <> colon
      <> nested (hardline <> sepMapBy hardline f (zip ix bs))
      <> case fb of (Just fb') -> pretty "_ ->" <+> align (pretty fb'); Nothing -> emptyDoc
    where
      f (i, e) = pretty i <+> pretty "->" <+> align (pretty e)

sepMapBy :: Doc ann -> (a -> Doc ann) -> [a] -> Doc ann
sepMapBy sep' f xs = concatWith (\a b -> a <> sep' <> b) (map f xs)
