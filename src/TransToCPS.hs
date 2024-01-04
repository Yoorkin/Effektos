{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TransToCPS (translate) where

import CPS
import Control.Monad.State.Lazy
import qualified Lambda as L

type Env = Int

uniqueName :: String -> State Env String
uniqueName n = do
  i <- get
  modify (+ 1)
  pure $ "^" ++ n ++ show i

trans :: L.Expr -> (Name -> State Env Term) -> State Env Term
trans (L.Var n) kont = kont n
trans (L.Abs x e) kont =
  do
    f <- uniqueName "f"
    k <- uniqueName "k"
    LetVal f <$> (Fn k x <$> trans e (pure . Continue k)) <*> kont f
trans (L.Let x e1 e2) kont =
  do
    j <- uniqueName "j"
    LetCont j x <$> trans e2 kont <*> trans e1 (pure . Continue j)
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
                LetCont k x <$> kont x <*> pure (Apply e1 k e2)
            )
      )
trans (L.Const c) kont = do
  constant <- uniqueName "const"
  let v = case c of
        L.Integer v -> I32 v
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
trans (L.Fix fs e') kont =
  let g ((n, (x, e)) : fs) acc = do
        x <- uniqueName x
        k <- uniqueName "k"
        e <- trans e (pure . Continue k)
        g fs ((n, Fn k x e) : acc)
      g [] acc = LetFns (reverse acc) <$> trans e' kont
   in g fs []

-- trans (L.Switch e case default) =
--   trans e (\e ->
--     let f (c:cs)  )

translate :: L.Expr -> Term
translate e = evalState (trans e (pure . Halt)) 0
