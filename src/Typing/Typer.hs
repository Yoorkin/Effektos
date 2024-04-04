{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Typing.Typer (typingProg) where

import Control.Monad (mapAndUnzipM, replicateM, zipWithM_)
import Control.Monad.State
import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Syntax2.AST as AST
import Typing.Builtin
import Typing.QualifiedNames
import Typing.Typedtree
import Util.CompileEnv

type TypeBindings = Map Name Type

type Constraint = (Type, Type)

data Context = Context
  { freshTypeList :: [Type],
    constraints :: [Constraint],
    typeInfoMap :: Map Name TypeInfo,
    valueInfoMap :: Map Name ValueInfo
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

findTypeInfo :: Name -> State Context (Maybe TypeInfo)
findTypeInfo n = gets (Map.lookup n . typeInfoMap)

findValueInfo :: Name -> State Context (Maybe ValueInfo)
findValueInfo n = gets (Map.lookup n . valueInfoMap)

insertValueInfo :: Name -> ValueInfo -> State Context ()
insertValueInfo n v = modify $ \ctx ->
  let info = valueInfoMap ctx
   in ctx {valueInfoMap = Map.insert n v info}

typingPattern :: TypeBindings -> AST.Pattern -> State Context (TypeBindings, Pattern)
typingPattern bindings (AST.PatVar n) = do
  ty <- freshType
  return (Map.insert n ty bindings, PatVar ty n)
typingPattern bindings (AST.PatConstr constr pats) = do
  (bindings', pats') <-
    foldlM
      ( \(bindings1, pats') pat -> do
          (bindings2, pat') <- typingPattern bindings1 pat
          return (bindings2, pat' : pats')
      )
      (bindings, [])
      pats
  info <- findValueInfo constr
  let constrTy = case info of
        Just (ConstrInfo _ _ ty) -> ty
        _ -> error $ "constructor '" ++ show constr ++ "' not found"
  let (paramTys, retTy) = splitArrowType constrTy
  zipWithM_ constraint paramTys (map typeOfPat pats')
  return (bindings', PatConstr retTy constr pats')
typingPattern bindings (AST.PatConstant c) =
  let ty = typeOfConstant c
   in return (bindings, PatConstant ty c)

typingExpr :: TypeBindings -> AST.Expr -> State Context Expr
typingExpr bindings (AST.Var n) =
  let ty = case Map.lookup n bindings of
        Just x -> x
        Nothing -> error $ "type of var '" ++ show n ++ "' not found"
   in return (Var ty n)
typingExpr bindings (AST.Fun pat body) =
  do
    (bindings', pat') <- typingPattern bindings pat
    body' <- typingExpr bindings' body
    let ty = arrowType (typeOfPat pat') (typeOfExpr body')
    return (Fun ty pat' body')
typingExpr bindings (AST.App f x) =
  do
    f' <- typingExpr bindings f
    x' <- typingExpr bindings x
    ty <- freshType
    let actualFnTy = typeOfExpr f'
    let expectedFnTy = arrowType (typeOfExpr x') ty
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
    tys <- replicateM (length binders) freshType
    let newBindings = Map.fromList (zip binders tys)
    let bindings' = newBindings `Map.union` bindings
    fns' <- mapM (typingFn bindings') fns
    zipWithM_ constraint tys (map typeOfFn fns')
    expr' <- typingExpr bindings' expr
    let ty = typeOfExpr expr'
    return (Fix ty binders fns' expr')
  where
    typingFn bindings1 (pat, body) = do
      (bindings2, pat') <- typingPattern bindings1 pat
      body' <- typingExpr bindings2 body
      constraint (typeOfPat pat') (typeOfExpr body')
      return (pat', body')
    typeOfFn (pat, body) = arrowType (typeOfPat pat) (typeOfExpr body)
typingExpr bindings (AST.If cond ifso ifnot) =
  do
    cond' <- typingExpr bindings cond
    ifso' <- typingExpr bindings ifso
    ifnot' <- typingExpr bindings ifnot
    ty <- freshType
    constraint (typeOfExpr ifso') ty
    constraint (typeOfExpr ifnot') ty
    constraint (typeOfExpr cond') boolType
    return (If ty cond' ifso' ifnot')
typingExpr bindings (AST.Match cond pats exprs) =
  do
    cond' <- typingExpr bindings cond
    (pats', exprs') <- mapAndUnzipM (typingCase bindings) (zip pats exprs)
    let patTys = map typeOfPat pats'
    let condTy = typeOfExpr cond'
    mapM_ (constraint condTy) patTys
    ty <- freshType
    let exprTys = map typeOfExpr exprs'
    mapM_ (constraint ty) exprTys
    return (Match ty cond' pats' exprs')
  where
    typingCase bindings1 (pat, expr) = do
      (bindings2, pat') <- typingPattern bindings1 pat
      expr' <- typingExpr bindings2 expr
      constraint (typeOfPat pat') (typeOfExpr expr')
      return (pat', expr')
typingExpr bindings (AST.Tuple elems) =
  do
    elems' <- mapM (typingExpr bindings) elems
    let ty = tupleType (map typeOfExpr elems')
    return (Tuple ty elems')
typingExpr bindings (AST.Prim prim args) = do
  do
    args' <- mapM (typingExpr bindings) args
    let primTy = typeOfPrim prim
    let argTys = map typeOfExpr args'
    let (paramTys, retTy) = splitArrowType primTy
    zipWithM_ constraint argTys paramTys
    return (Prim retTy prim args')
typingExpr _ (AST.Const c) =
  let ty = typeOfConstant c
   in return (Const ty c)
typingExpr bindings (AST.Sequence expr1 expr2) =
  do
    expr1' <- typingExpr bindings expr1
    expr2' <- typingExpr bindings expr2
    constraint (typeOfExpr expr1') unitType
    let ty = typeOfExpr expr2'
    return (Seq ty expr1' expr2')
typingExpr _ AST.Hole =
  Hole <$> freshType
typingExpr _ _ = error ""

typingAnno :: TypeBindings -> AST.Anno -> State Context Type
typingAnno bs (AST.AnnoVar var) = typingAnno bs (AST.AnnoTypeConstr var [])
typingAnno bs (AST.AnnoArrow a b) = typingAnno bs (AST.AnnoTypeConstr arrowName [a, b])
typingAnno bs (AST.AnnoTuple xs) = typingAnno bs (AST.AnnoTypeConstr (tupleTypeConstrName (length xs)) xs)
typingAnno bs (AST.AnnoTypeConstr constr annos) = do
  constr' <- findTypeInfo constr
  elems <- mapM (typingAnno bs) annos
  case constr' of
    Just (TypeConstrInfo n arity) | arity == length annos -> return (TypeConstr n elems)
    Nothing -> error $ "type " ++ show constr ++ " not found"

typingDecl :: TypeBindings -> AST.Decl -> State Context (Maybe Decl)
typingDecl _ (AST.Datatype {}) = return Nothing
typingDecl bindings (AST.TopValue n anno expr) =
  Just
    <$> case anno of
      Nothing -> TopBinding n <$> typingExpr bindings expr
      Just anno' -> do
        expr' <- typingExpr bindings expr
        ty <- typingAnno bindings anno'
        let exprTy = typeOfExpr expr'
        constraint ty exprTy
        return (TopBinding n expr')

scanTypes :: [AST.Decl] -> [(Name, TypeInfo)]
scanTypes = mapMaybe go
  where
    go (AST.Datatype n quants _) = Just (n, TypeConstrInfo n (length quants))
    go _ = Nothing

-- scanTopValue :: Map Name TypeInfo -> [AST.Decl] -> [(Name, ValueInfo)]
-- scanTopValue typeInfoMap = concatMap go
--   where
--     go (AST.TopValue n _ _) = [(n, DummyInfo)]
--     go (AST.Datatype _ _ constrs) = map (\(n, _) -> (n, ConstrInfo))

-- scanDecls :: [AST.Decl] -> Table
-- scanDecls decls = Table (Map.fromList (types ++ builtinTypes)) (Map.fromList values) (Map.fromList datatypes)
--   where
--     types = catMaybes $ flip map decls $ \case
--       (AST.Datatype n quants _) -> Just (n, TypeConstrInfo n (length quants))
--       _ -> Nothing

--     datatypes = catMaybes $ flip map decls $ \case
--       (AST.Datatype n quants constrs) -> Just (n, DataTypeInfo n quants (map fst constrs))
--       _ -> Nothing

--     values = flip concatMap decls $ \case
--       (AST.TopValue n _ _) -> [(n, DummyInfo)]
--       (AST.Datatype _ _ constrs) -> map (\(name, _) -> (name, DummyInfo)) constrs

makeConstrTy :: [TyVar] -> [Type] -> Type -> Type
makeConstrTy quants =
  case quants of
    [] -> go
    _ ->
      -- (TypeForall quants .) . go
      \x y -> TypeForall quants (go x y)
  where
    go [param] ret = TypeConstr arrowName [param, ret]
    go (param : ps) ret = TypeConstr arrowName [param, go ps ret]

typingDatatype :: AST.Decl -> State Context ()
typingDatatype (AST.Datatype n quants constrs) = do
  mapM_ typingConstr constrs
  where
    quants' = map Unsolved quants
    dataTy = TypeConstr n (map TypeVar quants')
    typingConstr (constr, annos) = do
      argTys <- mapM (typingAnno Map.empty) annos
      let ty = makeConstrTy quants' argTys dataTy
      let constrInfo = ConstrInfo constr (length annos) ty
      insertValueInfo constr constrInfo

typingProg' :: TypeBindings -> AST.Program -> State Context Program
typingProg' bindings (AST.Program decls) = do
  let datatypeDecls = filter (\case (AST.Datatype {}) -> True; _ -> False) decls
  mapM_ typingDatatype datatypeDecls
  decls' <- mapM (typingDecl bindings) decls
  return (Program (catMaybes decls'))

typingProg :: AST.Program -> Program
typingProg prog@(AST.Program decls) =
  let (prog', context') = runState (typingProg' bindings prog) context
      solutions = unify (constraints context') []
      rewritten = rewriteProg (Map.fromList solutions) prog'
   in rewritten
  where
    typeInfoMap = Map.fromList (scanTypes decls ++ builtinTypes)
    valueInfoMap = Map.empty
    bindings = Map.empty
    freshTypes = [TypeVar . Unsolved $ synName (v : show (i :: Int)) | v <- ['a' .. 'z'], i <- [0 ..]]
    context = Context freshTypes [] typeInfoMap valueInfoMap

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
