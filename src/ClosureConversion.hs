module ClosureConversion where

import Util(free)
import CPS
import Control.Monad.State.Lazy
import Control.Lens (transformM)

transClosure :: Term -> Term 
transClosure t = evalState (transformM f t) 0
     where 
      f (LetVal n (Fn k xs l) m) = 
        let 
          code = n ++ "'" 
          env = n ++ "_env"
          freeVars = free l
          l' = wrapProj l (zip [1..] freeVars) env
        in
        pure $ LetVal code (Fn k (env:xs) l')
        (LetVal n (Tuple (code : freeVars)) m)
      f (Apply g k xs) = state (\i -> 
          let fname = g ++ "^" ++ show i
              closure = g ++ "'"
              r = LetSel fname 0 closure (Apply fname k (closure:xs))
          in (r,i + 1)) 
      f x = pure x
      wrapProj x ((i,var):vars) env = wrapProj (LetSel var i env x) vars env
      wrapProj x [] _ = x


