{-# LANGUAGE DeriveDataTypeable #-}

module CPS where

import Control.Lens (Traversal')
import Control.Lens.Plated
import Data.Data
import Data.Data.Lens
import qualified Lambda as L

type Name = String

data Value
  = Var Name
  | I32 Int
  | Unit
  | Tuple [Value]
  | Cont Name Term -- k e
  | Fn Name Name Term -- k x e
  deriving (Eq, Ord, Show, Read, Data)

data Term
  = LetVal Name Value Term
  | LetSel Name Int Value Term
  | LetCont Name Name Term Term
  | LetFns [(Name, Value)] Term
  | Continue Value Value
  | Apply Value Value Value
  | LetPrim Name L.Primitive [Value] Term
  | Switch Value [Term]
  | Halt Value
  deriving (Eq, Ord, Show, Read, Data)

instance Plated Value where
  plate = uniplate

instance Plated Term where
  plate = uniplate

value :: Traversal' Term Value
value f = goExpr
  where
    goValue x = case x of
      (Var _) -> f x
      (I32 _) -> f x
      Unit -> f x
      (Tuple xs) -> (Tuple <$> traverse f xs) *> f x
      (Cont n t) -> (Cont n <$> goExpr t) *> f x
      (Fn n1 n2 t) -> (Fn n1 n2 <$> goExpr t) *> f x
    goExpr x = case x of
      (LetVal n v t) -> LetVal n <$> goValue v <*> goExpr t
      (LetSel n i v t) -> LetSel n i <$> goValue v <*> goExpr t
      (LetCont n1 n2 t1 t2) -> LetCont n1 n2 <$> goExpr t1 <*> goExpr t2
      (LetFns fns t1) -> LetFns <$> traverse (\(a, b) -> (,) a <$> goValue b) fns <*> goExpr t1
      (Continue v1 v2) -> Continue <$> goValue v1 <*> goValue v2
      (Apply v1 v2 v3) -> Apply <$> goValue v1 <*> goValue v2 <*> goValue v3
      (LetPrim n p vs t) -> LetPrim n p <$> traverse goValue vs <*> goExpr t
      (Switch v ts) -> Switch <$> goValue v <*> traverse goExpr ts
      (Halt v) -> Halt <$> goValue v
