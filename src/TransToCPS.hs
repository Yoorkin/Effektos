{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TransToCPS (translate) where

import CPS
import qualified Constant as Const
import qualified Lambda as L
import CompileEnv hiding (Name)


uniqueName :: String -> CompEnv String
uniqueName = freshWithBase

trans :: L.Expr -> (Name -> CompEnv Term) -> CompEnv Term
trans (L.Var n) kont = kont n
trans (L.Abs x e) kont =
  do
    f <- uniqueName "f"
    k <- uniqueName "k"
    LetVal f <$> (Fn k Nothing [x] <$> trans e (pure . Continue k Nothing)) <*> kont f
trans (L.Let x e1 e2) kont =
  do
    j <- uniqueName "j"
    LetCont j Nothing x <$> trans e2 kont <*> trans e1 (pure . Continue j Nothing)
trans (L.App e1 e2) kont =
  do
    k <- uniqueName "k"
    x <- uniqueName "x"
    trans
      e1
      ( \e1 ->
          trans
            e2
            ( \e2 ->
                LetCont k Nothing x <$> kont x <*> pure (Apply e1 k Nothing [e2])
            )
      )
trans (L.Const c) kont = do
  constant <- uniqueName "c"
  let v = case c of
        Const.Integer v -> I32 v
        Const.Unit -> Unit
        Const.Boolean v -> if v then I32 1 else I32 0
  LetVal constant v <$> kont constant
trans (L.Tuple xs) kont = do
  tuple <- uniqueName "t"
  let f (e : es) acc = trans e (\x -> f es (x : acc))
      f [] acc = LetVal tuple (Tuple $ reverse acc) <$> kont tuple
   in f xs []
trans (L.Select i e) kont =
  do
    x <- uniqueName "x"
    trans e (\e -> LetSel x i e <$> kont x)
trans (L.PrimOp op es) kont =
  let f (e : es) acc = trans e (\x -> f es (x : acc))
      f [] acc = do
        r <- uniqueName "r"
        LetPrim r op (reverse acc) <$> kont r
   in f es []
trans (L.Constr rep e) kont =
  case rep of
    L.TaggedRep _ ->
      trans e kont
trans (L.Decon rep e) kont =
  case rep of
    L.TaggedRep _ ->
      trans (L.Select 1 e) kont
trans (L.Fix ns fs e') kont =
  let g ((n, (x, e)) : fs) acc = do
        x <- uniqueName x
        k <- uniqueName "k"
        e <- trans e (pure . Continue k Nothing)
        g fs ((n, Fn k Nothing [x] e) : acc)
      g [] acc = LetFns (reverse acc) <$> trans e' kont
   in g (zip ns fs) []
trans (L.Handle e hs) kont =
  let g ((h, x, hk, e) : hs') = do
        hf <- uniqueName "h"
        k <- uniqueName "k"
        hbody <- trans e (pure . Continue k Nothing)
        LetVal hf (Fn k Nothing [x, hk] hbody) <$> (Handle h hf <$> g hs')
      g [] = trans e kont
   in g hs
trans (L.Switch cond cases fallback) kont = do
  k <- uniqueName "k"
  x <- uniqueName "x"
  let (index, branches) = unzip cases
  let g branch = trans branch (pure . Continue k Nothing)
  let branches' = mapM g branches
  let fallback' = mapM g fallback
  trans
    cond
    ( \cond' ->
        LetCont k Nothing x
          <$> kont x
          <*> (Switch cond' index <$> branches' <*> fallback')
    )

-- trans (L.Resume c a) kont = trans c (\c' -> trans a (pure . Continue c' Nothing))
-- should be function application instead of `Continue`.
-- Because the handler function is a normal function, not a continuation

translate :: L.Expr -> CompEnv Term
translate e = trans e (pure . Halt)
