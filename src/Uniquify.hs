{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Uniquify (uniquifyTerm) where

import CompileEnv
import qualified Data.Map as Map
import Data.Maybe
import Lambda

type Subst = Map.Map Name Name

uniquify :: Subst -> Expr -> CompEnv Expr
uniquify st =
  \case
    (Var n) -> pure $ Var (applySubst n st)
    (Abs n e) -> do
      n' <- uniName n
      Abs n' <$> go (appendSubst n n' st) e
    (Let n e m) -> do
      n' <- uniName n
      let st' = appendSubst n n' st
      Let n' <$> go st' e <*> go st' m
    (App l m) -> App <$> go st l <*> go st m
    (Fix ns fns e) -> do
      ns' <- mapM uniName ns
      let st1 = appendSubsts ns ns' st
      let aux (x, m) = do
            x' <- uniName x
            m' <- go (appendSubst x x' st1) m
            pure (x', m')
      Fix ns' <$> mapM aux fns <*> go st1 e
    (Lambda.Const c) -> pure $ Lambda.Const c
    (Tuple xs) -> Tuple <$> mapM (go st) xs
    (Select i e) -> Select i <$> go st e
    (PrimOp p es) -> PrimOp p <$> mapM (go st) es
    (Constr r e) -> Constr r <$> go st e
    (Switch e cases fallback) ->
      Switch <$> go st e <*> mapM (\(a, b) -> (a,) <$> go st b) cases <*> mapM (go st) fallback
    (Handle e effs) ->
      let aux (eff, n1, n2, l) = do
            n1' <- uniName n1
            n2' <- uniName n2
            l' <- go (appendSubst n1 n1' (appendSubst n2 n2' st)) l
            pure
              ( eff,
                n1',
                n2',
                l'
              )
       in Handle <$> go st e <*> mapM aux effs
    (Resume l m) -> Resume <$> go st l <*> go st m
    (Raise eff x) -> Raise eff <$> go st x
  where
    appendSubst = Map.insert
    appendSubsts [] [] st' = st'
    appendSubsts (old : olds) (new : news) st' = appendSubst old new (appendSubsts olds news st')
    applySubst old st' =
      fromMaybe old (Map.lookup old st')
    go = uniquify

uniquifyTerm :: Expr -> CompEnv Expr
uniquifyTerm = uniquify Map.empty
