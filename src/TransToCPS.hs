{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TransToCPS (translate) where

import CPS
import CompileEnv
import qualified Constant as Const
import qualified Lambda as L

uniqueName :: String -> CompEnv Name
uniqueName = freshStr

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
        k <- uniqueName "k"
        e <- trans e (pure . Continue k Nothing)
        g fs ((n, Fn k Nothing [x] e) : acc)
      g [] acc = LetFns (reverse acc) <$> trans e' kont
   in g (zip ns fs) []
trans (L.Switch cond cases _) kont = do
  let (ix, branches) = unzip cases
  trans
    cond
    ( \cond' ->
        let f acc =
              \case
                (b : bs) -> do
                  branchK <- uniqueName "branch"
                  branchX <- uniqueName "x"
                  c <- freshStr "c"
                  LetCont branchK Nothing branchX
                    <$> trans b kont
                    <*> f (LetVal c Unit (Continue branchK Nothing c) : acc) bs
                [] -> pure $ Switch cond' ix (reverse acc)
         in f [] branches
    )

-- The handler function is a normal function, not a continuation
trans (L.Handle e hds) kont =
  let aux :: [(L.Effect, Name, Name, L.Expr)] -> (Name -> CompEnv Term) -> CompEnv Term
      aux [] kont' = trans e kont'
      aux ((effect, hx, hk, l) : hds') kont' = do
        handler <- freshStr "handler"
        k <- freshStr "k"
        l' <- trans l (pure . Continue k Nothing)
        m' <- aux hds' (fmap (PopHdl effect) . kont')
        pure $
          LetVal
            handler
            (Fn k Nothing [hx, hk] l')
            ( PushHdl
                effect
                handler
                m'
            )
   in aux hds kont
trans (L.Raise eff arg) kont = do
  k <- freshStr "k"
  x <- freshStr "x"
  trans arg (\arg' -> LetCont k Nothing x <$> kont x <*> pure (Raise eff k Nothing [arg']))

translate :: L.Expr -> CompEnv Term
translate e = trans e (pure . Halt)
