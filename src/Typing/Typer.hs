{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Typing.Typer (typingProg) where

import Common.Name
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Syntax2.AST as AST
import Typing.Constraint
import Typing.Typedtree

typingProg :: AST.Program -> Program
typingProg prog =
  let (prog', constraints') = typingConstraint prog
      solutions = unify constraints' []
      rewritten = rewriteProg (Map.fromList solutions) prog'
   in -- pTraceShow prog' $
      -- pTraceShow (constraints context')
      rewritten

unify :: [Constraint] -> [Constraint] -> [Constraint]
unify [] acc = acc
unify (c@(a, b) : cs) acc = case c of
  (TypeVar x, TypeVar y)
    | x == y -> unify cs acc
    | isUnsolved x && not (isUnsolved y) -> unify ((b, a) : cs) acc
  (TypeVar x, _)
    | x `notElem` free b ->
        let p = rewriteConstraint a b in unify (p cs) ((a, b) : p acc)
  (_, TypeVar y)
    | y `notElem` free a ->
        let p = rewriteConstraint b a in unify (p cs) ((b, a) : p acc)
  (TypeConstr _ es1, TypeConstr _ es2) ->
    unify (zip es1 es2 ++ cs) acc
  _ -> error $ "cannot unify " ++ show c ++ "\n <!> constraints: \n" ++ show cs ++ show acc

rewriteConstraint :: Type -> Type -> [Constraint] -> [Constraint]
rewriteConstraint a b = fmap (\(x, y) -> (replace' x, replace' y))
  where
    replace' = rewriteType (Map.singleton a b)

rewriteType :: Map Type Type -> Type -> Type
rewriteType subst = \case
  a@(TypeVar _)
    | Just b <- Map.lookup a subst -> b
    | otherwise -> a
  (TypeConstr constr xs) -> TypeConstr constr (map (rewriteType subst) xs)
  (TypeForall bounded ty) ->
    TypeForall bounded (rewriteType subst' ty)
    where
      nfv = map TypeVar bounded
      subst' = Map.filterWithKey (\k _ -> k `notElem` nfv) subst

rewritePattern :: Map Type Type -> Pattern -> Pattern
rewritePattern subst = \case
  (PatVar ty str) -> PatVar (goTy ty) str
  (PatCon ty constr pats) -> PatCon (goTy ty) constr (map go pats)
  (PatLit ty c) -> PatLit (goTy ty) c
  _ -> error ""
  where
    goTy = rewriteType subst
    go = rewritePattern subst

rewriteProg :: Map Type Type -> Program -> Program
rewriteProg subst (Program decls) = Program $ map go decls
  where
    go (TopBinding n e) = TopBinding n (rewriteExpr subst e)

rewriteExpr :: Map Type Type -> Expr -> Expr
rewriteExpr subst = \case
  (Var ty str) -> Var (goTy ty) str
  (Lam ty pat body) -> Lam (goTy ty) (goPat pat) (go body)
  (App ty f x) -> App (goTy ty) (go f) (go x)
  (Let ty pat body expr) -> Let (goTy ty) (goPat pat) (go body) (go expr)
  (Case ty cond pats exprs) -> Case (goTy ty) (go cond) (map goPat pats) (map go exprs)
  (Prim ty op) -> Prim (goTy ty) op
  (Lit ty c) -> Lit (goTy ty) c
  (Con ty c) -> Con (goTy ty) c
  where
    go = rewriteExpr subst
    goTy = rewriteType subst
    goPat = rewritePattern subst
    goFns = map $ \(pat, body) -> (goPat pat, go body)
