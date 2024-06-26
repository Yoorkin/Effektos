{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Syntax2.Uniquify (uniquifyProg) where

import Control.Monad (zipWithM)
import Data.Foldable (foldlM)
import qualified Data.Map as Map
import Data.Maybe
import Syntax2.AST
import Common.CompileEnv
import Common.Name

type Subst = Map.Map Name Name

append :: Name -> Name -> Subst -> Subst
append = Map.insert

appends :: [Name] -> [Name] -> Subst -> Subst
appends _ [] st = st
appends [] _ st = st
appends (x : xs) (y : ys) st = appends xs ys (append x y st)

subst :: (Ord k) => k -> Map.Map k k -> k
subst old = fromMaybe old . Map.lookup old

uniName :: (Monad m) => Name -> CompEnvT m Name
uniName = toUnique

uniquifyPatterns :: Subst -> [Pattern] -> CompEnv (Subst, [Pattern])
uniquifyPatterns st = foldlM
    ( \(st1, acc) pat -> do
        (st2, pat') <- uniquifyPattern st1 pat
        return (st2, acc ++ [pat'])
    )
    (st, [])

uniquifyPattern :: Subst -> Pattern -> CompEnv (Subst, Pattern)
uniquifyPattern st (PatVar n) = do
  n' <- uniName n
  return (append n n' st, PatVar n')
uniquifyPattern st (PatConstr constr pats) = do
  let constr' = subst constr st
  (st', pats') <- uniquifyPatterns st pats
  return (st', PatConstr constr' pats')
uniquifyPattern st pat@(PatConstant {}) = return (st, pat)
uniquifyPattern st (PatTuple pats) = do
  (st', pats') <- uniquifyPatterns st pats
  return (st', PatTuple pats')
uniquifyPattern st (PatOr pat1 pat2) = do
  (st1, pat1') <- uniquifyPattern st pat1
  (st2, pat2') <- uniquifyPattern st1 pat2
  return (st2, PatOr pat1' pat2')
uniquifyPattern st PatWildCard = return (st, PatWildCard)
uniquifyPattern st (PatAnno pat anno) = do
  (st', pat') <- uniquifyPattern st pat
  anno' <- uniquifyAnno st' anno
  return (st', PatAnno pat' anno')

uniquify :: Subst -> Expr -> CompEnv Expr
uniquify st (Var n) = return (Var $ subst n st)
uniquify st (Fun p e) = do
  (st', p') <- uniquifyPattern st p
  e' <- uniquify st' e
  return (Fun p' e')
uniquify st (Let p e m) = do
  (st', p') <- uniquifyPattern st p
  e' <- uniquify st' e
  m' <- uniquify st' m
  return (Let p' e' m')
uniquify st (App l m) = App <$> uniquify st l <*> uniquify st m
uniquify st (Fix ns fns e) = do
  ns' <- mapM uniName ns
  let st1 = foldl (\acc (a, b) -> append a b acc) st (zip ns ns')
  let aux (p, m) = do
        (st2, p') <- uniquifyPattern st p
        m' <- uniquify st2 m
        pure (p', m')
  Fix ns' <$> mapM aux fns <*> uniquify st1 e
uniquify st (If a b c) = If <$> uniquify st a <*> uniquify st b <*> uniquify st c
uniquify st (Match cond pats exprs) = do
  cond' <- uniquify st cond
  (pats', exprs') <- unzip <$> zipWithM go pats exprs
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


uniquifyAnno :: Subst -> Anno -> CompEnv Anno
uniquifyAnno st = go
  where
    go :: Anno -> CompEnv Anno
    go (AnnoVar n) = return (AnnoVar (subst n st))
    go (AnnoArrow a b) = AnnoArrow <$> go a <*> go b
    go (AnnoTuple xs) = AnnoTuple <$> mapM go xs
    go (AnnoTypeConstr n xs) = AnnoTypeConstr (subst n st) <$> mapM go xs
    go (AnnoForall ns anno) = do
      ns' <- mapM uniName ns
      let st' = appends ns ns' st
      anno' <- uniquifyAnno st' anno
      return (AnnoForall ns' anno')

uniquifyTopBinding :: Subst -> TopBinding -> CompEnv TopBinding
uniquifyTopBinding st (TopBinding n anno expr) =
  TopBinding (subst n st) <$> mapM (uniquifyAnno st) anno <*> uniquify st expr

uniquifyDatatype :: Subst -> Datatype -> CompEnv Datatype
uniquifyDatatype st (Datatype n quants constrs) = do
  quants' <- mapM uniName quants
  let st' = appends quants quants' st
  constrs' <- processConstrs st' constrs
  return (Datatype (subst n st') quants' constrs')
  where
    processConstrs st1 = mapM (processConstr st1)

    processConstr st1 (constr, annos) =
      (subst constr st1,) <$> mapM (uniquifyAnno st1) annos

scanDeclNames :: [Datatype] -> [TopBinding] -> [Name]
scanDeclNames datatypes bindings = concatMap g datatypes ++ concatMap f bindings
  where
    f (TopBinding n _ _) = [n]
    g (Datatype n _ constrs) = n : map fst constrs

uniquifyProg :: Program -> CompEnv Program
uniquifyProg (Program datatypes values) = do
  let topNames = scanDeclNames datatypes values
  topNames' <- mapM uniName topNames
  let st = Map.fromList (zip topNames topNames')
  datatypes' <- mapM (uniquifyDatatype st) datatypes
  values' <- mapM (uniquifyTopBinding st) values
  return (Program datatypes' values')
