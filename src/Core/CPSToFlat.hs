{-# LANGUAGE LambdaCase #-}

module Core.CPSToFlat (hoistToFlat) where
  
import Core.CPS
import Util.CompileEnv
import qualified JS.Flat as F

valueToFlat :: Value -> F.Value
valueToFlat = \case
  (Var n) -> F.Var n
  (I32 v) -> F.I32 v
  Unit -> F.Unit
  (Tuple xs) -> F.Tuple xs
  _ -> error ""

toFlat :: Term -> ([F.Fn], [F.Binding], F.Expr)
toFlat (LetVal x (Fn k (Just env) zs l) m) =
  let (f1, b1, e1) = toFlat l
      (f2, b2, e2) = toFlat m
      fx = F.Fn x ([env, k] ++ zs) b1 e1
   in (f1 ++ f2 ++ [fx], b2, e2)
toFlat (LetVal x v l) =
  let (fs, bs, e) = toFlat l
      b = F.Binding x (valueToFlat v)
   in (fs, b : bs, e)
toFlat (LetSel x i y k) =
  let (fs, bs, e) = toFlat k
      b = F.Binding x (F.Proj i y)
   in (fs, b : bs, e)
toFlat (LetCont k x l m) =
  let (fs1, bs1, e1) = toFlat l
      (fs2, bs2, e2) = toFlat m
      fk = F.Fn k [x] bs1 e1
   in (fs1 ++ fs2 ++ [fk], bs2, e2)
toFlat (LetFns fns l) =
  let g ((n, Fn k (Just env) xs m) : remains) acc =
        let (f1, b1, e1) = toFlat m
            fx = F.Fn n (env : k : xs) b1 e1
         in g remains (fx : f1 ++ acc)
      g [] acc = reverse acc
      g _ _ = error ""
      (f2, b2, e2) = toFlat l
   in (f2 ++ g fns [], b2, e2)
toFlat (Continue k x) =
  ([], [], F.Apply k [x])
toFlat (Apply f k (Just env) xs) =
  ([], [], F.Apply f (env : k : xs))
toFlat (LetPrim x op ys l) =
  let (fs, bs, e) = toFlat l
      b = F.Binding x (F.PrimOp op ys)
   in (fs, b : bs, e)
toFlat (Switch cond index branches fallback) =
  let g (l : ls) fs bs es =
        let (f1, b1, e1) = toFlat l
         in g ls (f1 ++ fs) (bs ++ b1) (e1 : es)
      g [] fs bs es = (reverse fs, bs, reverse es)
   in let (fs, bs, es) = g branches [] [] []
          (fs', bs', fb') = case fallback of
            Nothing -> ([], [], Nothing)
            Just fb ->
              let (a, b, c) = toFlat fb
               in (a, b, Just c)
       in (fs' ++ fs, bs' ++ bs, F.Switch cond index es fb')
toFlat (Halt n) = ([], [], F.Exit n)
toFlat (Handle l hdls) =
  let (fs, bs, e) = toFlat l
   in (fs, bs, F.Handle e hdls)
toFlat (Raise h k args) =
  ([], [], F.Raise h (k : args))
toFlat x = error (show x)

hoistToFlat :: Term -> CompEnv F.Program
hoistToFlat t = F.Program <$> fm <*> pure fs
  where
    (fs, bs, e) = toFlat t
    fm = F.Fn <$> freshStr "main" <*> pure [] <*> pure bs <*> pure e
