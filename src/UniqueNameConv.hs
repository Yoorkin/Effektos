{-# LANGUAGE MonoLocalBinds #-}
module UniqueNameConv where
import Lambda
import Data.Generics.Uniplate.Data
import Control.Monad.State.Lazy as State
import qualified Data.Map as Map

-- uniquify :: Expr -> State Int Expr
-- uniquify ([n],gen) = gen n
-- uniquify ([n,e],gen) = gen n e
-- uniquify ([n,e1,e2],gen) = gen n e1 e2


-- addUnique :: String -> State (Int, Map.Map String String) String
-- addUnique old = 
--       do
-- 				(i,m) <- State.get
-- 				let new = old ++ show i
-- 				modify (\(i,m) -> (i+1, Map.insertWith (flip (++)) old [new] m))
-- 				pure new

-- uniquifyNames :: Expr -> Expr
-- uniquifyNames e = evalState (transformM f x) (0,Map.fromList [])
--   where 
-- 			f (Var n) = Var <$> (Map.lookup n <$> snd <$> get)
-- 			f (Abs n e) =  
-- 			f x = pure x

traverseM :: Monad m => Uniplate a => (a -> m a) -> (a -> m a) -> a -> m a
traverseM f g x = g =<< (descendM (traverseM f g) =<< f x)

visit :: Expr -> IO Expr
visit = traverseM f g

f,g :: Expr -> IO Expr
f e = do 
        putStrLn $ "enter: " ++ show e
        pure e
g e = do
        putStrLn $ "leave: " ++ show e
        pure e

