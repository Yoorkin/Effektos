{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BetaReduction where

import CPS
import Control.Lens (transformOn)
import Control.Lens.Plated (rewriteM, universe)
import Control.Monad.State.Lazy
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Debug.Trace
import Lambda (Primitive (..))
import Text.Pretty.Simple (pPrint, pShow)
import Uniquify
import Util (def, var, usage)
import qualified Lambda as L
import Prelude hiding (lookup)
import Data.Text.Lazy(unpack)
import Control.Lens (transformMOn)

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
  (LetCont n _ _ _) -> [(n, t)]
  (LetFns fns _) -> map ((,t) . fst) fns
  (LetPrim n _ _ _) -> [(n, t)]
  _ -> []

collect :: Term -> Bindings
collect t = Map.fromList (concatMap bindings $ universe t)

valueOfLetVal :: Term -> Maybe Value
valueOfLetVal (LetVal _ v _) = Just v
valueOfLetVal _ = Nothing

reduceM :: Term -> State Bindings Term
reduceM = rewriteM g
  where
    g :: Term -> State Bindings (Maybe Term)
    g t = do
      bs <- get
      r <- case t of
        -- beta cont
        (Continue k y) -> pure $
          case Map.lookup k bs of
            Just (LetCont _ x c _) -> do
              Just $ inline [(x, y)] c
            _ -> Nothing
        -- beta fun
        (Apply f j y) -> pure $
          case Map.lookup f bs of
            Just (LetVal _ (Fn k x c) _) -> Just $ inline [(x, y), (k, j)] c
            _ -> Nothing
        -- beta pair
        (LetSel y i x c) -> pure $
          case Map.lookup x bs of
            Just (LetVal _ (Tuple elems) _) ->
              let xi = elems !! i in Just $ inline [(y, xi)] c
            _ -> Nothing
        -- constant folding
        (LetPrim n op ns e) -> pure $
          case mapM
            (\x -> Map.lookup x bs >>= valueOfLetVal)
            ns of
            Just vs ->
              case (op, vs) of
                (Add2, [I32 a, I32 b]) -> Just (LetVal n (I32 $ a + b) e)
                (Sub2, [I32 a, I32 b]) -> Just (LetVal n (I32 $ a - b) e)
                _ -> Nothing
            _ -> Nothing
        _ -> pure Nothing
      let new = Map.fromList . bindings $ fromMaybe t r
      put (new `Map.union` bs)
      pure r

reduce :: Term -> Term
reduce t = t'
  where
    t' = evalState (reduceM t) (collect t)

type Census = Map.Map Name Int
type Env = Map.Map Name Value
type Subst = Map.Map Name Name

count census x = fromMaybe 1 (Map.lookup x census)
addEnv :: Env -> Name -> Value -> Env
addEnv env x v = Map.insert x v env
lookup env x = Map.lookup x env
applySubst s x = fromMaybe x (Map.lookup x s)
extendSubst st s t = Map.insert s t st

simpVal :: Census -> Env -> Subst -> Value -> State Int Value
simpVal census env s v = case v of
    Var n -> pure $ Var (applySubst s n)
    Tuple ns -> pure $ Tuple (map (applySubst s) ns)
    Cont n l -> Cont n <$> simp census env s l
    Fn k x l -> Fn k x <$> simp census env s l
    _ -> pure v

inst :: Term -> State Int Term
inst p =
    do
      u <- get
      let defs = Map.fromList $ zipWith (\a b -> (a, a ++ "_" ++ show b)) (def p) [u..]
      let f x = fromMaybe x (Map.lookup x defs)
      put (u + length defs)
      let r = transformOn var f p
      -- () <- traceM ("\n========= from ======\n" ++ show p ++ "\n====\n" ++ show r ++ "\n== END ==\n")
      pure r


simp :: Census -> Env -> Subst -> Term -> State Int Term
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
      let y' = applySubst s y in
      case lookup env y' of
        Just (Tuple elems) -> simp census env (extendSubst s x (elems !! i)) l
        _ -> LetSel x i y' <$> simp census env s l
    LetCont k x l m -> do
      l' <- simp census env s l
      case count census k of
        0 -> simp census env s m
        -- 1 -> simp census (addEnv env k (Cont x l')) s m
        _ -> LetCont k x l' <$> simp census (addEnv env k (Cont x l')) s m
    Continue k x ->
      let x' = applySubst s x in
      let k' = applySubst s k in
      case lookup env k' of
        Just (Cont y l) -> inst l >>= simp census env (extendSubst s y x')
        _ -> pure $ Continue k' x'
    Apply f k x ->
      let
        f' = applySubst s f
        k' = applySubst s k
        x' = applySubst s x
      in case lookup env f' of
           Just (Fn k1 x1 l) -> inst l >>= simp census env (extendSubst (extendSubst s x1 x') k1 k')
           _ -> pure $ Apply f' k' x'
    LetPrim n op ns t ->
      let fallback = simp census env s t in
      if count census n == 0 then fallback else
        let ns' = map (applySubst s) ns in
        case (op, mapM (lookup env) ns') of
            (Add2, Just [I32 a, I32 b]) ->
                let v = I32 $ a + b in LetVal n v <$> simp census (addEnv env n v) s t
            _ -> LetPrim n op ns' <$> fallback
    Halt n -> pure $
      Halt (applySubst s n)
    x -> pure x

simplify :: Term -> Term
simplify p = evalState (simp census Map.empty Map.empty p) 100
      where census = usage p




-- data Context 
--   = CVal Name Value 
--   | CSel Name Int Name
--   | CCont Name Name Term
--   | CFn Name Value
--   | CPrim Name L.Primitive [Name]
--   deriving Show

-- optimize :: Term -> State Bindings Term
-- optimize t =
--   case t of
--     (LetVal n v t1) -> do
--       t2 <- optimize t1
--       update n t2
--       pure (LetVal n v t2)
--     -- beta selection
--     (LetSel y i x c) ->
--       let t1 = case find x of
--             Just (LetVal _ (Tuple elems) _) -> inline [(y, elems !! i)] c
--             _ -> t
--        in update y t1 $> t1
--     -- beta cont
--     (Continue k y) -> 
--       case find k of
--         Just (LetCont j x c _) -> optimize $ inline [(x, y)] c
--         _ -> pure t
--     _ -> error ""
--   where
--     find n = get >>= Map.lookup n
--     update n t = modify (Map.insert n t)
