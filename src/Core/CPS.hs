{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Core.CPS where

import Control.Lens.Plated
import Data.Data
import Data.Data.Lens
import qualified Syntax.Constant as C
import Syntax.Primitive
import Util.CompileEnv

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
  | LetConts [(Name, Value)] Term
  | LetFns [(Name, Value)] Term
  | Continue Name Name
  | Apply Function Cont (Maybe Closure) [Argument]
  | LetPrim Name Primitive [Name] Term
  | Switch Name [C.Constant] [Term] (Maybe Term)
  | Handle Term [(Effect, Function)]
  | Raise Effect Cont [Argument] -- h k x
  | Halt Name
  deriving (Eq, Show, Ord, Read, Data)

instance Plated Value where
  plate = uniplate

instance Plated Term where
  plate = uniplate

type FnBinding = (Name, Value)

type ContBinding = (Name, Value)

hoisting :: Term -> Term
hoisting expr =
  let (fns, conts, e') = go expr
   in case fns of
        [] -> e'
        _ -> LetFns fns (wrapConts conts e')
  where
    wrapConts :: [ContBinding] -> Term -> Term
    wrapConts [] t = t
    wrapConts conts t = LetConts conts t

    go :: Term -> ([FnBinding], [ContBinding], Term)
    go (LetPrim n p xs e) =
      let (fns, conts, e') = go e
       in (fns, conts, LetPrim n p xs e')
    go (LetCont k x e1 e2) =
      let (fns1, conts1, e1') = go e1
          (fns2, conts2, e2') = go e2
          cont = (k, Cont x e1')
       in (fns1 ++ fns2, cont : conts1 ++ conts2, e2')
    go (LetFns funcs e) =
      let fns1 = aux [] funcs
          (fns2, conts2, e') = go e
       in (fns1 ++ fns2, conts2, e')
      where
        aux fns [] = fns
        aux fns ((n, Fn k env xs m) : remain) =
          let (fns', conts', m') = go m
           in aux ((n, Fn k env xs (wrapConts conts' m')) : fns' ++ fns) remain
    go (LetVal n v e) =
      let (fns, conts, e') = go e
       in case v of
            (Fn k env xs m) ->
              let (fns1, conts1, m') = go m
               in ((n, Fn k env xs (wrapConts conts1 m')) : fns1 ++ fns, conts, e')
            _ -> (fns, conts, LetVal n v e')
    go (LetSel n i x e) =
      let (fns, conts, e') = go e
       in (fns, conts, LetSel n i x e')
    go (Switch n cs es fb) =
      let (fnsLs, contsLs, es') = unzip3 (map go es)
          (fns, conts, fb') = case fmap go fb of
            Just (fns', cont', e') -> (fns', cont', Just e')
            Nothing -> ([], [], Nothing)
       in (concat (fns : fnsLs), concat (conts : contsLs), Switch n cs es' fb')
    go x = ([], [], x)
