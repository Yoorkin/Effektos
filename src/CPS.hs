{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CPS where

import CompileEnv
import qualified Constant as C
import Control.Lens.Plated
import Data.Data
import Data.Data.Lens
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Primitive

data Value
  = Var Name
  | I32 Int
  | Unit
  | Tuple [Name]
  | Cont Name Term -- env x e
  | Fn Name (Maybe Name) [Name] Term -- k env x e
  deriving (Eq, Show, Ord, Read, Data)

type Effect = String

type Cont = Name

type Function = Name

type Argument = Name

type Closure = Name

data Term
  = LetVal Name Value Term
  | LetSel Name Int Name Term -- TODO: Value to String, the Value should be (Var x)
  | LetCont Name Name Term Term
  | LetFns [(Name, Value)] Term
  | Continue Name Name
  | Apply Function Cont (Maybe Closure) [Argument]
  | LetPrim Name Primitive [Name] Term
  | Switch Name [C.Constant] [Term] (Maybe Term)
  | Handle Term [(Effect, Function)]
  | Raise Effect Cont [Argument] -- h k x
  | Halt Name
  deriving (Eq, Show, Ord, Read, Data)

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty (defaultLayoutOptions {layoutPageWidth = AvailablePerLine 50 1.0})

instance Plated Value where
  plate = uniplate

instance Plated Term where
  plate = uniplate

-- hoisting :: Term -> Term
-- hoisting expr@(LetPrim n p xs e) =
--     case hoisting e of
--       (LetFns fs e') -> LetFns fs (LetPrim n p xs e')
--       (LetVal f fn@(Fn {}) e') -> LetFns [(f, fn)] (LetPrim n p xs e')
--       _ -> expr
-- hoisting (LetCont k x e1 e) =
--     case hoisting e of

hoisting :: Term -> Term
hoisting expr = let (fns,e') = go expr in
                 case fns of [] -> e'
                             _ -> LetFns fns e'
  where
    go (LetPrim n p xs e) =
      let (fns, e') = go e
       in (fns, LetPrim n p xs e')
    go (LetCont k x e1 e2) =
      let (fns1, e1') = go e1
          (fns2, e2') = go e2
       in (fns1 ++ fns2, LetCont k x e1' e2')
    go (LetFns fns e) =
      let fns1 = concatMap aux fns
          (fns2, e') = go e
       in (fns1 ++ fns2, e')
      where
        aux (n, Fn k env xs m) =
          let (fns', m') = go m
           in ((n, Fn k env xs m') : fns')
    go (LetVal n v e) =
      let (fns, e') = go e in
      case v of
        (Fn k env xs m) ->
           let (fns1, m') = go m in
           ((n,Fn k env xs m'):fns1 ++ fns, e')
        _ -> (fns, LetVal n v e')
    go (LetSel n i x e) =
      let (fns, e') = go e in
      (fns, LetSel n i x e')
    go (Switch n cs es fb) =
      let (fnsls, es') = unzip (map go es)
          (fns,fb') = case fmap go fb of
                       Just (fns', e') -> (fns', Just e')
                       Nothing -> ([],Nothing)
       in (concat (fns:fnsls), Switch n cs es' fb')
    go x = ([], x)
