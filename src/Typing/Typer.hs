module Typing.Typer where

import Syntax.Constant as Constant
import Control.Monad.State
import Data.Map.Strict (Map,(!))
import qualified Data.Map.Strict as Map
import qualified Syntax.AST as AST
import Typing.Symbol
import Typing.Typedtree

type Bindings = Map String Type

type Constraint = (Type,Type)

data Context = Context
  { freshTypeList :: [Type],
    constraints :: [Constraint]
  }

freshType :: State Context Type
freshType =
  state $ \ctx ->
    case freshTypeList ctx of
      [] -> undefined
      (x : xs) -> (x, ctx {freshTypeList = xs})

-- freshTypes :: Int -> State Context TyVar
-- freshTypes i = do
--   ctx <- get
--   put (ctx { freshTypeList = drop i freshTypeList ctx })
--   state $ \ctx ->
--     case freshTypeList ctx of
--       [] -> undefined
--       (x : xs) -> (x, ctx {freshTyVars = xs})

constraint :: Type -> Type -> State Context ()
constraint a b = modify $ \ctx ->
  ctx {constraints = (a, b) : (constraints ctx)}

typingPattern :: Bindings -> AST.Pattern -> State Context (Bindings, Pattern)
typingPattern = error ""

typingExpr :: Bindings -> AST.Expr -> State Context Expr
typingExpr bindings (AST.Var n) =
  let ty = bindings ! n in return (Var ty n)
typingExpr bindings (AST.Fun pat body) =
  do
    (bindings', pat') <- typingPattern bindings pat
    body' <- typingExpr bindings' body
    let ty = makeArrowType (typeOfPat pat') (typeOfExpr body')
    return (Fun ty pat' body')
typingExpr bindings (AST.App f x) =
  do
    f' <- typingExpr bindings f
    x' <- typingExpr bindings x
    ty <- freshType
    let actualFnTy = typeOfExpr f'
    let expectedFnTy = makeArrowType (typeOfExpr x') ty
    constraint actualFnTy expectedFnTy
    return (App ty f' x')
typingExpr bindings (AST.Let pat body expr) =
  do
    (bindings', pat') <- typingPattern bindings pat
    body' <- typingExpr bindings body
    expr' <- typingExpr bindings' expr
    constraint (typeOfPat pat') (typeOfExpr body')
    let ty = typeOfExpr expr'
    return (Let ty pat' body' expr')
typingExpr bindings (AST.Fix binders fns expr) =
  do
    tys <- sequence (repeat freshType)
    let newBindings = Map.fromList (zip binders tys)
    let bindings' = newBindings `Map.union` bindings
    fns' <- mapM (typingFn bindings') fns
    expr' <- typingExpr bindings' expr
    let ty = typeOfExpr expr'
    return (Fix ty binders fns' expr')
  where
    typingFn bindings1 (pat, body) = do
      (bindings2, pat') <- typingPattern bindings1 pat
      body' <- typingExpr bindings1 body
      constraint (typeOfPat pat') (typeOfExpr body')
      return (pat', body')
typingExpr bindings (AST.If cond ifso ifnot) =
  do
    cond' <- typingExpr bindings cond
    ifso' <- typingExpr bindings ifso
    ifnot' <- typingExpr bindings ifnot
    ty <- freshType
    constraint (typeOfExpr ifso') ty
    constraint (typeOfExpr ifnot') ty
    constraint (typeOfExpr cond') (boolType)
    return (If ty cond' ifso' ifnot')
typingExpr bindings (AST.Match cond pats exprs) =
  do
    cond' <- typingExpr bindings cond
    (pats', exprs') <- unzip <$> mapM (typingCase bindings) (zip pats exprs)
    let patTys = map typeOfPat pats'
    let condTy = typeOfExpr cond'
    sequence $ zipWith constraint (repeat condTy) patTys
    ty <- freshType
    let exprTys = map typeOfExpr exprs'
    sequence $ zipWith constraint (repeat ty) exprTys
    return (Match ty cond' pats' exprs')
  where
    typingCase bindings (pat,expr) = do
      (bindings',pat') <- typingPattern bindings pat
      expr' <- typingExpr bindings' expr
      constraint (typeOfPat pat') (typeOfExpr expr')
      return (pat',expr')
typingExpr bindings (AST.Tuple elems) =
  do
    elems' <- mapM (typingExpr bindings) elems
    let ty = makeTupleType (map typeOfExpr elems')
    return (Tuple ty elems')
typingExpr bindings (AST.Prim prim args) = do
  do
    args' <- mapM (typingExpr bindings) args
    let primTy = typeOfPrim prim
    let argTys = map typeOfExpr args'
    let (paramTys,retTy) = splitArrowType primTy
    sequence $ zipWith constraint argTys paramTys
    return (Prim retTy prim args')
typingExpr _ (AST.Const c) =
    let ty =
          case c of
            (Constant.Integer _) -> intType
            (Constant.Boolean _) -> boolType
            Constant.Unit -> unitType
            _ -> error ""
    in return (Const ty c)
typingExpr bindings (AST.Sequence expr1 expr2) =
  do
    expr1' <- typingExpr bindings expr1
    expr2' <- typingExpr bindings expr2
    constraint (typeOfExpr expr1') unitType
    let ty = typeOfExpr expr2'
    return (Sequence ty expr1' expr2')
typingExpr _ AST.Hole =
  Hole <$> freshType
typingExpr _ _ = error ""

unify :: [Constraint] -> [Constraint] -> [Constraint]
unify [] acc = acc
unify (c@(a,b):cs) acc = case c of
    (TypeVar x, TypeVar y)
            | x == y -> unify cs acc
            | head x /= '\'' && head y == '\'' -> unify ((b,a):cs) acc
    (TypeVar x, _) | x `notElem` free b ->
            let p = rewriteConstraint a b in unify (p cs) ((a,b) : p acc)
    (_, TypeVar y) | y `notElem` free a ->
            let p = rewriteConstraint b a in unify (p cs) ((b,a) : p acc)
    (TypeConstr _ es1, TypeConstr _ es2)  ->
            unify (zip es1 es2 ++ cs) acc
    _ -> error $ "cannot unify " ++ show c ++ "\n <!> constraints: \n" ++ show cs ++ show acc

rewriteConstraint :: Type -> Type -> [Constraint] -> [Constraint]
rewriteConstraint a b cs = fmap (\(x,y) -> (replace' x, replace' y)) cs
        where replace' = rewriteType a b


rewriteType :: Type -> Type -> Type -> Type
rewriteType a b = go
    where go ty = case ty of
            (TypeVar _) -> if ty == a then b else ty
            (TypeConstr sym xs) -> TypeConstr sym (fmap go xs)
            (TypeForall bounded ty') -> if a `elem` map TypeVar bounded then ty
                                        else TypeForall bounded (go ty')

