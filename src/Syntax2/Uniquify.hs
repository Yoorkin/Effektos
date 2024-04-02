{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Syntax2.Uniquify (uniquifyTerm) where

import Util.CompileEnv
import Syntax2.AST
import qualified Data.Map as Map
import Data.Maybe
import Data.Foldable(foldlM)
import Control.Monad (zipWithM)

type Subst = Map.Map Name Name

uniquifyPatterns :: Subst -> [Pattern] -> CompEnv (Subst,[Pattern])
uniquifyPatterns st pats =
          foldlM (\(st1,acc) pat -> do 
                             (st2,pat') <- uniquifyPattern st1 pat
                             return (st2,pat':acc)) (st,[]) pats 

uniquifyPattern :: Subst -> Pattern -> CompEnv (Subst,Pattern)
uniquifyPattern st (PatVar n) = do
         n' <- uniName n
         return (append n n' st, PatVar n')  
uniquifyPattern st (PatConstr constr pats) = do
         constr' <- uniName constr
         (st',pats') <- uniquifyPatterns st pats
         return (st', PatConstr constr' pats')
uniquifyPattern st pat@(PatConstant {}) = return (st,pat)
uniquifyPattern st (PatTuple pats) = do 
         (st',pats') <- uniquifyPatterns st pats
         return (st', PatTuple pats')
uniquifyPattern st (PatOr pat1 pat2) = do
         (st1, pat1') <- uniquifyPattern st pat1
         (st2, pat2') <- uniquifyPattern st1 pat2
         return (st2, PatOr pat1' pat2')
uniquifyPattern st PatWildCard = return (st,PatWildCard)

uniquify :: Subst -> Expr -> CompEnv Expr
uniquify st (Var n) = return (Var $ subst n st)
uniquify st (Fun p e) = do
          (st',p') <- uniquifyPattern st p
          e' <- uniquify st' e
          return (Fun p' e')
uniquify st (Let p e m) = do
        (st',p') <- uniquifyPattern st p
        e' <- uniquify st' e
        m' <- uniquify st' m
        return (Let p' e' m')
uniquify st (App l m) = App <$> uniquify st l <*> uniquify st m
uniquify st (Fix ns fns e) = do
        ns' <- mapM uniName ns
        let st1 = foldl (\acc (a,b) -> append a b acc) st (zip ns ns') 
        let aux (p, m) = do
              (st2, p') <- uniquifyPattern st p
              m' <- uniquify st2 m
              pure (p', m')
        Fix ns' <$> mapM aux fns <*> uniquify st1 e
uniquify st (If a b c) = If <$> uniquify st a <*> uniquify st b <*> uniquify st c
uniquify st (Match cond pats exprs) = do
        cond' <- uniquify st cond
        (pats',exprs') <- unzip <$> zipWithM go pats exprs
        return (Match cond' pats' exprs')
      where
        go pat expr = do
           (st', pat') <- uniquifyPattern st pat
           expr' <- uniquify st' expr
           return (pat', expr')
uniquify st (Tuple exprs) = Tuple <$> mapM (uniquify st) exprs
uniquify st (Prim prim expr) = Prim prim <$> mapM (uniquify st) expr
uniquify st (Anno expr anno) = Anno <$> uniquify st expr <*> pure anno
uniquify _ expr@(Const {}) = return expr
uniquify st (Sequence e1 e2) = Sequence <$> uniquify st e1 <*> uniquify st e2
uniquify _ Hole = return Hole

append :: Name -> a -> Map.Map Name a -> Map.Map Name a
append = Map.insert

subst :: Ord k => k -> Map.Map k k -> k
subst old = fromMaybe old . Map.lookup old

uniquifyTerm :: Expr -> CompEnv Expr
uniquifyTerm = uniquify Map.empty
