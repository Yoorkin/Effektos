{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BetaReduction where

import CPS
import Control.Lens (transformMOn, transformOn)
import Control.Lens.Plated (rewriteM, universe)
import Control.Monad.State.Lazy
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (unpack)
import Debug.Trace
import Primitive
import qualified Lambda as L
import Text.Pretty.Simple (pPrint, pShow)
import Uniquify
import Util (def, usage, var)
import Prelude hiding (lookup)
import Data.Bifunctor (Bifunctor(second))
import GHC.Base (assert)
import CompileEnv hiding (Name)

-- replace :: Name -> Name -> Term -> Term
-- replace s t = transformOn var f
--   where
--     f x
--       | x == s = t
--       | otherwise = x

inline :: [(Name, Name)] -> Term -> Term
inline ps t = transformOn var f t
  where
    defs = def t
    replace = Map.fromList $ ps ++ zip defs (zipWith (\a b -> a ++ "_" ++ show b) defs [0 ..])
    f x = fromMaybe x (Map.lookup x replace)

type Bindings = Map.Map Name Term

bindings :: Term -> [(Name, Term)]
bindings t = case t of
  (LetVal n _ _) -> [(n, t)]
  (LetSel n _ _ _) -> [(n, t)]
  (LetCont n _ _ _ _) -> [(n, t)]
  (LetFns fns _) -> map ((,t) . fst) fns
  (LetPrim n _ _ _) -> [(n, t)]
  _ -> []

collect :: Term -> Bindings
collect t = Map.fromList (concatMap bindings $ universe t)

type Census = Map.Map Name Int

type Env = Map.Map Name Value

type Subst = Map.Map Name Name

count census x = fromMaybe 1 (Map.lookup x census)

addEnv :: Env -> Name -> Value -> Env
addEnv env x v = Map.insert x v env

lookup env x = Map.lookup x env

applySubst s x = fromMaybe x (Map.lookup x s)

extendSubst st s t = Map.insert s t st

extendSubsts :: Subst -> [(Name, Name)] -> Subst
extendSubsts st [] = st
extendSubsts st ((s, t) : xs) = extendSubsts (extendSubst st s t) xs

simpVal :: Census -> Env -> Subst -> Value -> CompEnv Value
simpVal census env s v = case v of
  Var n -> pure $ Var (applySubst s n)
  Tuple ns -> pure $ Tuple (map (applySubst s) ns)
  Cont n mn l -> Cont n mn <$> simp census env s l
  Fn k _ x l -> Fn k Nothing x <$> simp census env s l
  _ -> pure v

inst :: Term -> CompEnv Term
inst p =
  do
    let defs = def p
    ndefs <- mapM freshWithBase defs 
    u <- get
    let f x = fromMaybe x (Map.lookup x (Map.fromList $ zip defs ndefs))
    let r = transformOn var f p
    pure r

simp :: Census -> Env -> Subst -> Term -> CompEnv Term
simp census env s p =
  -- traceWith (\r -> if r == p then "" else
  --                  "\n========================= from ==========================\n"
  --                   ++ show p
  --                   ++ "\n-----> env\n"
  --                   ++ unpack (pShow env)
  --                   ++ "\n-----> subst\n"
  --                   ++ unpack (pShow s)
  --                   ++ "\n-------------------- to -----------------\n" ++ show r ++ "\n=======") $
  case p of
    LetVal x v l ->
      if count census x == 0
        then simp census env s l
        else do
          v' <- simpVal census env s v
          LetVal x v' <$> simp census (addEnv env x v') s l
    LetSel x i y l ->
      let y' = applySubst s y
       in case lookup env y' of
            Just (Tuple elems) -> simp census env (extendSubst s x (elems !! i)) l
            _ -> LetSel x i y' <$> simp census env s l
    LetCont k _ x l m -> do
      l' <- simp census env s l
      case count census k of
        0 -> simp census env s m
        -- 1 -> simp census (addEnv env k (Cont x l')) s m
        _ -> LetCont k Nothing x l' <$> simp census (addEnv env k (Cont Nothing x l')) s m
    LetFns fns m -> do
      fns' <- mapM (\(x,b) -> (x,) <$> simpVal census env s b) fns
      m' <- simp census env s m
      pure $ LetFns fns' m'
    Continue k _ x ->
      let x' = applySubst s x
       in let k' = applySubst s k
           in case lookup env k' of
                Just (Cont _ y l) -> inst l >>= simp census env (extendSubst s y x')
                _ -> pure $ Continue k' Nothing x'
    Apply f k _ xs ->
      let f' = applySubst s f
          k' = applySubst s k
          xs' = map (applySubst s) xs
       in case lookup env f' of
            Just (Fn k1 _ xs1 l) ->
              inst l >>= simp census env (extendSubst (extendSubsts s (zip xs1 xs')) k1 k')
            _ -> pure $ Apply f' k' Nothing xs'
    LetPrim n op ns t ->
      let fallback = simp census env s t
       in if count census n == 0
            then fallback
            else
              let ns' = map (applySubst s) ns
               in case (op, mapM (lookup env) ns') of
                    (Add2, Just [I32 a, I32 b]) ->
                      let v = I32 $ a + b in LetVal n v <$> simp census (addEnv env n v) s t
                    _ -> LetPrim n op ns' <$> fallback
    Handle h f e -> 
      let h' = applySubst s h
          f' = applySubst s f
          e' = simp census env s e
       in Handle h' f' <$> e'

    Halt n ->
      pure $
        Halt (applySubst s n)
    x -> pure x

simplify' :: Term -> Term
simplify' p = evalState (simp census Map.empty Map.empty p) mkCompStates
  where
    census = usage p

simplify :: Term -> Term
simplify t = find $ iterate simplify' t
    where find (a:b:xs) | a == b = b
                        | otherwise = find (b:xs)
          find _ = error "unreachable"

