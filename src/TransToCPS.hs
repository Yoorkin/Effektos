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

type CurrentContinue = Name

transl :: L.Expr -> CurrentContinue -> (Name -> CurrentContinue -> CompEnv Term) -> CompEnv Term
transl expression currentFuncK kont =
  case expression of
    (L.Var n) -> kont n currentFuncK
    (L.Let x (L.Abs y e1) e2) ->
      do
        k <- uniqueName "k"
        func <- Fn k Nothing [y] <$> transl e1 k (\e' _ -> pure $ Continue k e')
        LetVal x func <$> transl e2 currentFuncK kont
    (L.Abs x e) ->
      do
        f <- uniqueName "f"
        k <- uniqueName "k"
        func <- Fn k Nothing [x] <$> transl e k (\e' _ -> pure $ Continue k e')
        LetVal f func <$> kont f currentFuncK
    (L.Let x e1 e2) ->
      do
        j <- uniqueName "j"
        transl e1 currentFuncK (\e1' cc -> LetCont j x <$> transl e2 cc kont <*> pure (Continue j e1'))
    (L.App e1 e2) ->
      do
        k <- uniqueName "k"
        x <- uniqueName "x"
        transl e1 currentFuncK $ \e1' currentFuncK ->
          transl e2 currentFuncK $ \e2' currentFuncK ->
            LetCont k x <$> kont x currentFuncK <*> pure (Apply e1' k Nothing [e2'])
    (L.Const c) -> do
      constant <- uniqueName "c"
      let v = case c of
            Const.Integer v -> I32 v
            Const.Unit -> Unit
            Const.Boolean v -> if v then I32 1 else I32 0
      LetVal constant v <$> kont constant currentFuncK
    (L.Tuple xs) -> do
      tuple <- uniqueName "t"
      let f (e : es) acc = transl e currentFuncK (\x _ -> f es (x : acc))
          f [] acc = LetVal tuple (Tuple $ reverse acc) <$> kont tuple currentFuncK
       in f xs []
    (L.Select i e) -> do
      x <- uniqueName "x"
      transl e currentFuncK $ \e currentFuncK -> LetSel x i e <$> kont x currentFuncK
    (L.PrimOp op es) ->
      let f (e : es) acc = transl e currentFuncK (\x _ -> f es (x : acc))
          f [] acc = do
            r <- uniqueName "r"
            LetPrim r op (reverse acc) <$> kont r currentFuncK
       in f es []
    (L.Constr rep e) ->
      case rep of
        L.TaggedRep _ ->
          transl e currentFuncK kont
    (L.Fix ns fs e') ->
      let g ((n, (x, e)) : fs) acc = do
            k <- uniqueName "k"
            e <- transl e k (\e' currentFuncK -> pure $ Continue currentFuncK e')
            g fs ((n, Fn k Nothing [x] e) : acc)
          g [] acc = LetFns (reverse acc) <$> transl e' currentFuncK kont
       in g (zip ns fs) []
    (L.Switch cond cases fallback) -> do
      let (ix, branches) = unzip cases
      transl cond currentFuncK $ \cond' currentFuncK ->
        let f acc =
              \case
                (b : bs) -> do
                  branchK <- uniqueName "branch"
                  branchX <- uniqueName "x"
                  c <- freshStr "c"
                  LetCont branchK branchX
                    <$> transl b currentFuncK kont
                    <*> f (LetVal c Unit (Continue branchK c) : acc) bs
                [] -> case fallback of 
                        Nothing -> pure $ Switch cond' ix (reverse acc) Nothing
                        Just fb -> do
                          branchK <- uniqueName "fallback"
                          branchX <- uniqueName "x"
                          c <- freshStr "c"
                          let fb' = LetVal c Unit (Continue branchK c)
                          LetCont branchK branchX 
                            <$> transl fb currentFuncK kont
                            <*> pure (Switch cond' ix (reverse acc) (Just fb'))
         in f [] branches 

    -- The handler function is a normal function, not a continuation
    (L.Handle (L.App func arg) hds) -> do
      exitK <- uniqueName "exitK"
      x <- uniqueName "x"
      transl func currentFuncK $ \func' currentFuncK ->
        transl arg currentFuncK $ \arg' currentFuncK ->
          LetCont exitK x
            <$> kont x currentFuncK
            <*> let aux [] acc = pure (Handle (Apply func' exitK Nothing [arg']) acc)
                    aux ((effect, hx, hk, l) : hds') acc = do
                      handler <- freshStr "handler"
                      k <- freshStr "k"
                      l' <- transl l k (\l' currentFuncK -> pure $ Continue currentFuncK l')
                      m' <- aux hds' ((effect, handler) : acc)
                      pure $
                        LetVal
                          handler
                          (Fn k Nothing [hx, hk] l')
                          m'
                 in aux hds []
    (L.Raise eff arg) -> do
      f <- freshStr "resume"
      k <- freshStr "k"
      x <- freshStr "x"
      resumeFunc <- Fn k Nothing [x] <$> kont x k
      transl arg currentFuncK (\arg' currentFuncK -> pure (LetVal f resumeFunc (Raise eff currentFuncK [arg', f])))
    (L.Resume f arg) ->
      transl f currentFuncK $ \f' currentFuncK -> do
        k <- freshStr "k"
        x <- freshStr "x"
        LetCont k x
          <$> kont x currentFuncK
          <*> transl arg currentFuncK (\arg' _ -> pure $ Apply f' k Nothing [arg'])

translate :: L.Expr -> CompEnv Term
translate e = do
  abortK <- freshStr "k"
  x <- freshStr "x"
  LetCont abortK x (Halt x) <$> transl e abortK (\z cc -> pure $ Continue cc z)
