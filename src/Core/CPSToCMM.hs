{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Core.CPSToCMM where

import CMM.CMM
import qualified Core.CPS as CPS
import Util.CompileEnv
import qualified Syntax.Constant as Constant

translate :: CPS.Term -> CompEnv Program
translate term =
  let (fns, term1) =
        case term of
          (CPS.LetFns fns' t) -> (fns', t)
          _ -> ([], term)
      -- fns = []
      -- term1 = term
      (conts, term2) =
        case term1 of
          (CPS.LetConts conts' t) -> (conts', t)
          _ -> ([], term1)
   in do
        carg <- freshStr "carg"
        fns' <- mapM translFn fns
        let conts' = map (toBlock Nothing carg) conts
        let expr = toExpr Nothing carg Nothing term2
        pure $ Program fns' (Define carg expr) conts'

translFn :: (Name, CPS.Value) -> CompEnv Fn
translFn (n, CPS.Fn k (Just env) args t) = do
  let (conts, e) =
        case t of
          (CPS.LetConts conts' e') -> (conts', e')
          _ -> ([], t)
  carg <- freshStr "carg"
  pure $ Fn n (env : args) (Define carg (toExpr (Just k) carg Nothing e)) (map (toBlock (Just k) carg) conts)
translFn _ = error "invalid input"

type FnKont = Name

type ContArg = Name

toBlock :: Maybe FnKont -> ContArg -> (Name, CPS.Value) -> Block
toBlock fnk carg (n, CPS.Cont x t) = (n, toExpr fnk carg (Just x) t)
toBlock _ _ _ = error "invalid input"

toExpr :: Maybe FnKont -> ContArg -> Maybe Name -> CPS.Term -> Expr
toExpr fnk carg contX t =
  let go = toExpr fnk carg contX
      subst x = case contX of
        Nothing -> x
        Just x' -> if x == x' then carg else x
   in case t of
        (CPS.Continue k x)
          | Just fnk' <- fnk, k == fnk' -> Return (subst x)
          | otherwise -> Assign carg (Variable (subst x)) (Jump k)
        (CPS.Apply f k (Just env) args)
          | Just fnk' <- fnk, k == fnk' -> Call carg f (map subst (env : args)) (Return carg)
          | otherwise -> Call carg f (map subst (env : args)) (Jump k)
        (CPS.LetVal n v l) ->
          case v of
            CPS.I32 i -> Define n (Assign n (Integer i) (go l))
            CPS.Unit -> Define n (Assign n Unit (go l))
            CPS.Var n2 -> Define n (Assign n (Variable (subst n2)) (go l))
            CPS.Tuple xs ->
              Define n (Malloc n (length xs) (initialize (zip [0 ..] (map subst xs))))
              where
                initialize [] = go l
                initialize ((i, x) : xs') = Mutate n i x (initialize xs')
            CPS.Cont {} -> error "invalid input"
            CPS.Fn {} -> error "invalid input"
        (CPS.LetPrim n op xs l) -> Define n (PrimOp n op (map subst xs) (go l))
        (CPS.LetSel n1 i n2 l) -> Define n1 (Select n1 i (subst n2) (go l))
        (CPS.Switch n cs es fb) ->
          Switch n (zip (map toConst cs) (map go es)) (fmap go fb)
        -- (CPS.Handle t hdls) -> error ""
        -- (CPS.Raise eff k xs) | Just fnk' <- fnk, k == fnk' -> Call carg f xs (Return reg)
        --                      | otherwise -> Call reg f xs (Jump k)
        (CPS.Halt x) -> Return (subst x)
        (CPS.LetCont {}) -> error ""
        (CPS.LetConts {}) -> error ""
        (CPS.LetFns {}) -> error ""

toConst :: Constant.Constant -> Value
toConst (Constant.Integer i) = Integer i
toConst (Constant.String s) = String s
toConst Constant.Unit = Unit
