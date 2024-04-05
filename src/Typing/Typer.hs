{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Typing.Typer (typingProg) where

import Common.Name
import Control.Monad (mapAndUnzipM, replicateM, zipWithM_)
import Control.Monad.State
import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Syntax2.AST as AST
import Typing.Builtin
import qualified Typing.Builtin as Builtin
import Typing.QualifiedNames
import Typing.Typedtree


type TypeMap = Map Name TypeInfo

type ValueMap = Map Name ValueInfo

type Constraint = (Type, Type)

data Context = Context
  { freshTypeList :: [Type],
    constraints :: [Constraint]
  }

instance Show Context where
  show = show . constraints

freshType :: State Context Type
freshType =
  state $ \ctx ->
    case freshTypeList ctx of
      [] -> undefined
      (x : xs) -> (x, ctx {freshTypeList = xs})

constraint :: Type -> Type -> State Context ()
constraint a b = modify $ \ctx ->
  ctx {constraints = (a, b) : constraints ctx}

typingPattern :: ValueMap -> TypeMap -> AST.Pattern -> State Context (ValueMap, Pattern)
typingPattern values _ (AST.PatVar n) = do
  ty <- freshType
  return (Map.insert n (LocalInfo ty) values, PatVar ty n)
typingPattern values types (AST.PatConstr constr pats) = do
  (values', pats') <-
    foldlM
      ( \(values1, pats') pat -> do
          (values2, pat') <- typingPattern values1 types pat
          return (values2, pats' ++ [pat'])
      )
      (values, [])
      pats
  let info = Map.lookup constr values
  let constrTy = case info of
        Just (ConstrInfo _ ty) -> ty
        _ -> error $ "constructor '" ++ show constr ++ "' not found"
  let (paramTys, retTy) = splitArrowType constrTy
  zipWithM_ constraint paramTys (map typeOfPat pats')
  return (values', PatConstr retTy constr pats')
typingPattern bindings _ (AST.PatConstant c) =
  let ty = typeOfConstant c
   in return (bindings, PatConstant ty c)

typingExpr :: ValueMap -> TypeMap -> AST.Expr -> State Context Expr
typingExpr values _ (AST.Var n) =
  let ty = case Map.lookup n values of
        Just (LocalInfo x) -> x
        Just (ConstrInfo _ x) -> x
        Just (ValueInfo x) -> x
        Nothing -> error $ "type of var '" ++ show n ++ "' not found"
   in return (Var ty n)
typingExpr values types (AST.Fun pat body) =
  do
    (values', pat') <- typingPattern values types pat
    body' <- typingExpr values' types body
    let ty = arrowType (typeOfPat pat') (typeOfExpr body')
    return (Fun ty pat' body')
typingExpr values types (AST.App f x) =
  do
    f' <- typingExpr values types f
    x' <- typingExpr values types x
    ty <- freshType
    let actualFnTy = typeOfExpr f'
    let expectedFnTy = arrowType (typeOfExpr x') ty
    constraint actualFnTy expectedFnTy
    return (App ty f' x')
typingExpr values types (AST.Let pat body expr) =
  do
    (values', pat') <- typingPattern values types pat
    body' <- typingExpr values types body
    expr' <- typingExpr values' types expr
    constraint (typeOfPat pat') (typeOfExpr body')
    let ty = typeOfExpr expr'
    return (Let ty pat' body' expr')
typingExpr values types (AST.Fix binders fns expr) =
  do
    tys <- replicateM (length binders) freshType
    let newValues = Map.fromList (zip binders (map LocalInfo tys))
    let values' = newValues `Map.union` values
    fns' <- mapM (typingFn values') fns
    zipWithM_ constraint tys (map typeOfFn fns')
    expr' <- typingExpr values' types expr
    let ty = typeOfExpr expr'
    return (Fix ty binders fns' expr')
  where
    typingFn values1 (pat, body) = do
      (values2, pat') <- typingPattern values1 types pat
      body' <- typingExpr values2 types body
      constraint (typeOfPat pat') (typeOfExpr body')
      return (pat', body')
    typeOfFn (pat, body) = arrowType (typeOfPat pat) (typeOfExpr body)
typingExpr values types (AST.If cond ifso ifnot) =
  do
    cond' <- typingExpr values types cond
    ifso' <- typingExpr values types ifso
    ifnot' <- typingExpr values types ifnot
    ty <- freshType
    constraint (typeOfExpr ifso') ty
    constraint (typeOfExpr ifnot') ty
    constraint (typeOfExpr cond') boolType
    return (If ty cond' ifso' ifnot')
typingExpr values types (AST.Match cond pats exprs) =
  do
    cond' <- typingExpr values types cond
    (pats', exprs') <- mapAndUnzipM (typingCase values) (zip pats exprs)
    let patTys = map typeOfPat pats'
    let condTy = typeOfExpr cond'
    mapM_ (constraint condTy) patTys
    ty <- freshType
    let exprTys = map typeOfExpr exprs'
    mapM_ (constraint ty) exprTys
    return (Match ty cond' pats' exprs')
  where
    typingCase values1 (pat, expr) = do
      (values2, pat') <- typingPattern values1 types pat
      expr' <- typingExpr values2 types expr
      constraint (typeOfPat pat') (typeOfExpr expr')
      return (pat', expr')
typingExpr values types (AST.Tuple elems) =
  do
    elems' <- mapM (typingExpr values types) elems
    let ty = tupleType (map typeOfExpr elems')
    return (Tuple ty elems')
typingExpr values types (AST.Prim prim args) = do
  do
    args' <- mapM (typingExpr values types) args
    let primTy = typeOfPrim prim
    let argTys = map typeOfExpr args'
    let (paramTys, retTy) = splitArrowType primTy
    zipWithM_ constraint argTys paramTys
    return (Prim retTy prim args')
typingExpr _ _ (AST.Const c) =
  let ty = typeOfConstant c
   in return (Const ty c)
typingExpr values types (AST.Sequence expr1 expr2) =
  do
    expr1' <- typingExpr values types expr1
    expr2' <- typingExpr values types expr2
    constraint (typeOfExpr expr1') unitType
    let ty = typeOfExpr expr2'
    return (Seq ty expr1' expr2')
typingExpr _ _ AST.Hole =
  Hole <$> freshType
typingExpr _ _ _ = error ""

typingAnno :: TypeMap -> AST.Anno -> State Context Type
typingAnno types (AST.AnnoVar var) = typingAnno types (AST.AnnoTypeConstr var [])
typingAnno types (AST.AnnoArrow a b) = typingAnno types (AST.AnnoTypeConstr arrowName [a, b])
typingAnno types (AST.AnnoTuple xs) = typingAnno types (AST.AnnoTypeConstr (tupleTypeConstrName (length xs)) xs)
typingAnno types (AST.AnnoForall quants anno) = typingAnno types' anno
  where
    types' = Map.union (Map.fromList $ zip quants (map QuantInfo quantTys)) types
    quantTys = map (TypeVar . Unsolved) quants
typingAnno types (AST.AnnoTypeConstr constr annos) = do
  elems <- mapM (typingAnno types) annos
  case Map.lookup constr types of
    Just (QuantInfo x) -> return x
    Just (TypeConstrInfo arity)
      | arity == length annos -> return (TypeConstr constr elems)
      | otherwise -> error "type constructor arity didn't match"
    Nothing -> error $ "type " ++ show constr ++ " not found"

scanTypes :: [AST.Datatype] -> [(Name, TypeInfo)]
scanTypes = map go
  where
    go (AST.Datatype n quants _) = (n, TypeConstrInfo (length quants))

makeConstrTy :: [TyVar] -> [Type] -> Type -> Type
makeConstrTy quants =
  case quants of
    [] -> go
    _ ->
      -- (TypeForall quants .) . go
      \x y -> TypeForall quants (go x y)
  where
    go [] ret = go [unitType] ret -- for special case: Constructor without payloads
    go [param] ret = TypeConstr arrowName [param, ret]
    go (param : ps) ret = TypeConstr arrowName [param, go ps ret]

-- collect toplevel ValueInfo in TopBinding
scanTopBindings :: [AST.TopBinding] -> State Context [(Name, ValueInfo)]
scanTopBindings [] = return []
scanTopBindings (AST.TopBinding n _ _ : remain) = do
  ty <- freshType
  values <- scanTopBindings remain
  return ((n, ValueInfo ty) : values)

-- collect ValueInfo in constructors of datatype
scanDatatypes :: TypeMap -> [AST.Datatype] -> State Context [(Name, ValueInfo)]
scanDatatypes _ [] = return []
scanDatatypes types (AST.Datatype n quants constrs : remain) = do
  let types' = Map.fromList (zip quants (map QuantInfo quantTys)) `Map.union` types
  r <- scanConstrs types' constrs
  rs <- scanDatatypes types' remain
  return (r ++ rs)
  where
    quantTys = map (TypeVar . Unsolved) quants
    dataTy = TypeConstr n quantTys

    scanConstrs :: TypeMap -> [(Constr, [AST.Anno])] -> State Context [(Name, ValueInfo)]
    scanConstrs _ [] = return []
    scanConstrs types ((constr, annos) : remain) = do
      argTys <- mapM (typingAnno types) annos
      let ty = makeConstrTy (map Unsolved quants) argTys dataTy
      let constrInfo = ConstrInfo (length annos) ty
      rs <- scanConstrs types remain
      return ((constr, constrInfo) : rs)

typingDecl :: ValueMap -> TypeMap -> AST.TopBinding -> State Context TopBinding
typingDecl values types (AST.TopBinding n anno expr) = do
  expr' <- typingExpr values types expr
  let exprTy = typeOfExpr expr'
  case anno of
    Nothing -> return ()
    Just anno' -> do
      ty <- typingAnno types anno'
      constraint ty exprTy
      return ()
  let info = Map.lookup n values
  let ty = case info of
        Just (ValueInfo x) -> x
        Just (ConstrInfo _ x) -> x
  constraint ty exprTy
  return (TopBinding n expr')

typingProg' :: AST.Program -> State Context Program
typingProg' (AST.Program datatypes bindings) = do
  -- collect types
  let builtinTypes = Builtin.builtinTypes
  let topTypes = scanTypes datatypes
  let types = Map.fromList (builtinTypes ++ topTypes)

  -- collect values (toplevel bindings, constructors)
  let builtinValues = []
  topValues <- scanTopBindings bindings
  constrValues <- scanDatatypes types datatypes
  let values = Map.fromList (builtinValues ++ topValues ++ constrValues)

  -- collect constraints
  bindings' <- mapM (typingDecl values types) bindings
  return (Program bindings')

typingProg :: AST.Program -> Program
typingProg prog =
  let (prog', context') = runState (typingProg' prog) context
      solutions = unify (constraints context') []
      rewritten = rewriteProg (Map.fromList solutions) prog'
   in -- pTraceShow prog' $
      -- pTraceShow (constraints context')
      rewritten
  where
    freshTypes = [TypeVar . Unsolved $ synName (v : show (i :: Int)) | v <- ['a' .. 'z'], i <- [0 ..]]
    context = Context freshTypes []

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
  (PatConstr ty constr pats) -> PatConstr (goTy ty) constr (map go pats)
  (PatConstant ty c) -> PatConstant (goTy ty) c
  (PatTuple ty elems) -> PatTuple (goTy ty) (map go elems)
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
  (Fun ty pat body) -> Fun (goTy ty) (goPat pat) (go body)
  (App ty f x) -> App (goTy ty) (go f) (go x)
  (Let ty pat body expr) -> Let (goTy ty) (goPat pat) (go body) (go expr)
  (Fix ty binders fns expr) -> Fix (goTy ty) binders (goFns fns) (go expr)
  (If ty cond ifso ifnot) -> If (goTy ty) (go cond) (go ifso) (go ifnot)
  (Match ty cond pats exprs) -> Match (goTy ty) (go cond) (map goPat pats) (map go exprs)
  (Tuple ty elems) -> Tuple (goTy ty) (map go elems)
  (Prim ty op xs) -> Prim (goTy ty) op (map go xs)
  (Const ty c) -> Const (goTy ty) c
  (Seq ty a b) -> Seq (goTy ty) (go a) (go b)
  (Hole ty) -> Hole (goTy ty)
  where
    go = rewriteExpr subst
    goTy = rewriteType subst
    goPat = rewritePattern subst
    goFns = map $ \(pat, body) -> (goPat pat, go body)
