{-# LANGUAGE LambdaCase #-}
module HoistToFlat where

import CPS
import qualified Flat as F

valueToFlat :: Value -> F.Value
valueToFlat = \case
                (Var n) -> F.Var n
                (I32 v) -> F.I32 v
                Unit -> F.Unit
                (Tuple xs) -> F.Tuple xs
                _ -> error ""

toFlat :: Term -> ([F.Fn], [F.Binding], F.Expr)
toFlat (LetVal x (Fn k (Just env) zs l) m) = 
     let (f1,b1,e1) = toFlat l 
         (f2,b2,e2) = toFlat m
         fx = F.Fn x ([env,k] ++ zs) b1 e1
      in (f1++f2++[fx], b2, e2)
toFlat (LetVal x v l) = 
     let (fs,bs,e) = toFlat l
         b = F.Binding x (valueToFlat v)
      in (fs, b:bs, e)
toFlat (LetSel x i y k) =
     let (fs,bs,e) = toFlat k 
         b = F.Binding x (F.Proj i y)
      in (fs,b:bs,e)
toFlat (LetCont k (Just env) x l m) =
     let (fs1,bs1,e1) = toFlat l
         (fs2,bs2,e2) = toFlat m
         fk = F.Fn k [env,x] bs1 e1  
      in (fs1 ++ fs2 ++ [fk], bs2, e2)
toFlat (LetFns fns l) = 
      let g ((n,Fn k (Just env) xs m):remains) acc =
              let (f1,b1,e1) = toFlat m
                  fx = F.Fn n (env:k:xs) b1 e1
               in g remains (fx:f1 ++ acc)
          g [] acc = reverse acc
          g _ _ = error "" 
          (f2,b2,e2) = toFlat l
       in (f2 ++ g fns [], b2, e2) 
toFlat (Continue k (Just env) x) = 
     ([],[],F.Apply k [env,x])
toFlat (Apply f k (Just env) xs) =
     ([],[],F.Apply f (env:k:xs))
toFlat (LetPrim x op ys l) =
     let (fs,bs,e) = toFlat l
         b = F.Binding x (F.PrimOp op ys)
      in (fs,b:bs,e)
toFlat (Switch cond cases) = 
     let g (l:ls) fs bs es =
           let (f1,b1,e1) = toFlat l
            in g ls (f1++fs) (b1++bs) (e1:es)
         g [] fs bs es = (reverse fs, reverse bs, reverse es)
      in let (fs,bs,es) = g cases [] [] [] in
          (fs,bs,F.Switch cond es)
toFlat (Halt n) = ([],[],F.Exit n)
toFlat _ = error ""

hoistToFlat :: Term -> F.Program
hoistToFlat t = F.Program fm fs
      where (fs,bs,e) = toFlat t
            fm = F.Fn "main" [] bs e
