{-# LANGUAGE DeriveDataTypeable #-}

module CPS where

import Control.Lens (Traversal')
import Control.Lens.Plated
import Data.Data
import Data.Data.Lens
import qualified Lambda as L
import Prettyprinter
import Prettyprinter.Render.String (renderString)

type Name = String

data Value
  = Var Name
  | I32 Int
  | Unit
  | Tuple [Name]
  | Cont Name Term -- k e
  | Fn Name Name Term -- k x e
  deriving (Eq, Ord, Read, Data)

data Term
  = LetVal Name Value Term
  | LetSel Name Int Name Term -- TODO: Value to String, the Value should be (Var x)
  | LetCont Name Name Term Term
  | LetFns [(Name, Value)] Term
  | Continue Name Name
  | Apply Name Name Name
  | LetPrim Name L.Primitive [Name] Term
  | Switch Name [Term]
  | Halt Name
  deriving (Eq, Ord, Read, Data)

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty defaultLayoutOptions

instance Show Value where
  show = renderDoc . pretty

instance Show Term where
  show = renderDoc . pretty

instance Plated Value where
  plate = uniplate

instance Plated Term where
  plate = uniplate

(</>) a b = a <> line <> b

(<//>) a b = a <> hardline <> b

nested = nest 2

instance Pretty Value where
  pretty (Var n) = pretty n
  pretty (I32 i) = parens $ pretty "i32" <+> pretty i
  pretty Unit = pretty "()"
  pretty (Tuple es) = lparen <> concatWith (\x y -> x <> comma <+> y) (map pretty es) <> rparen
  pretty (Cont n t) = group (pretty "λ" <> pretty n <> dot <> pretty t)
  pretty (Fn k x t) = group (parens $ pretty "λ" <+> pretty k <+> pretty x <+> dot <> nested (line <> pretty t))

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
                </> parens
                  ( pretty "select"
                      <+> pretty i
                      <+> pretty n'
                  )
            )
            </> pretty "in"
        )
        </> pretty t
  pretty (LetCont k x c t) =
    group $
      pretty "Letcont"
        <> nested
          ( softline
              <> pretty k
              <+> pretty x
              <+> pretty "="
              <> nested
                ( line
                    <> pretty c
                )
          )
          </> pretty "in"
          </> pretty t
  pretty (LetFns {}) = pretty "@"
  pretty (Continue k x) = group (parens $ pretty "Continue" <+> pretty k <+> pretty x)
  pretty (Apply f k x) = group (parens $ pretty f <+> pretty k <+> pretty x)
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
  pretty (Switch {}) = pretty "@"
  pretty (Halt e) = group (parens $ pretty "halt" </> pretty e)

-- value :: Traversal' Term Value
-- value f = goExpr
--   where
--     goValue x = case x of
--       (Var _) -> f x
--       (I32 _) -> f x
--       Unit -> f x
--       (Tuple xs) -> (Tuple <$> traverse f xs) *> f x
--       (Cont n t) -> (Cont n <$> goExpr t) *> f x
--       (Fn n1 n2 t) -> (Fn n1 n2 <$> goExpr t) *> f x
--     goExpr x = case x of
--       (LetVal n v t) -> LetVal n <$> goValue v <*> goExpr t
--       (LetSel n i v t) -> LetSel n i <$> goValue v <*> goExpr t
--       (LetCont n1 n2 t1 t2) -> LetCont n1 n2 <$> goExpr t1 <*> goExpr t2
--       (LetFns fns t1) -> LetFns <$> traverse (\(a, b) -> (,) a <$> goValue b) fns <*> goExpr t1
--       (Continue v1 v2) -> Continue <$> goValue v1 <*> goValue v2
--       (Apply v1 v2 v3) -> Apply <$> goValue v1 <*> goValue v2 <*> goValue v3
--       (LetPrim n p vs t) -> LetPrim n p <$> traverse goValue vs <*> goExpr t
--       (Switch v ts) -> Switch <$> goValue v <*> traverse goExpr ts
--       (Halt v) -> Halt <$> goValue v
