{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TransToCPS where

import CPS
import Control.Monad.State.Lazy
import qualified Lambda as L

addStamp :: String -> State Int String
addStamp n = do
  i <- get
  modify (+ 1)
  pure $ n ++ "" ++ show i

addStamps :: [String] -> State Int [String]
addStamps = mapM addStamp

trans :: L.Expr -> (Value -> State Int Term) -> State Int Term
trans (L.Var n) kont = addStamp n >>= kont . Var
trans (L.Abs x e) kont =
  do
    f <- addStamp "f"
    k <- addStamp "k"
    x <- addStamp x
    LetVal f <$> (Fn k x <$> trans e (pure . Continue (Var k))) <*> kont (Var f)
trans (L.Let x e1 e2) kont =
  do
    j <- addStamp "j"
    x <- addStamp x
    LetCont j x <$> trans e2 kont <*> trans e1 (pure . Continue (Var j))
trans (L.App e1 e2) kont =
  do
    k <- addStamp "k"
    x <- addStamp "x"
    trans
      e1
      ( \e1 ->
          trans
            e2
            ( \e2 ->
                LetCont k x <$> kont (Var x) <*> pure (Apply e1 (Var k) e2)
            )
      )
trans (L.Const c) kont =
  kont $ case c of
    L.Integer v -> I32 v
trans (L.Tuple xs) kont =
  let f (e : es) acc = trans e (\x -> f es (x : acc))
      f [] acc = kont (Tuple (reverse acc))
   in f xs []
trans (L.Select i e) kont =
  do
    x <- addStamp "x"
    trans e (\e -> LetSel x i e <$> kont (Var x))
trans (L.PrimOp op es) kont =
  let f (e : es) acc = trans e (\x -> f es (x : acc))
      f [] acc = do
        r <- addStamp "r"
        LetPrim r op (reverse acc) <$> kont (Var r)
   in f es []
trans (L.Constr rep e) kont =
  case rep of
    L.TaggedRep i ->
      trans
        e
        ( \e -> do
            c <- addStamp "c"
            LetVal c e <$> kont (Var c)
        )
trans (L.Decon rep e) kont =
  case rep of
    L.TaggedRep i ->
      trans (L.Select 1 e) kont

-- trans (L.Switch e case default) =
--   trans e (\e ->
--     let f (c:cs)  )

translate :: L.Expr -> Term
translate e = evalState (trans e (pure . Halt)) 0
