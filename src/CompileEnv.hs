module CompileEnv
  ( Stamp,
    Name,
    CompStates,
    CompEnvT,
    CompEnv,
    stamp,
    fresh,
    uniquify,
    freshStr,
    mkCompStates,
    freshWithBase,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (lift)
import Control.Monad.State.Lazy (State, StateT, get, put)
import Control.Comonad.Identity

type Stamp = Int

data Name
  = SynName String
  | UniName String Stamp
  | GenName Stamp
  deriving (Eq, Ord)

instance Show Name where
  show (SynName n) = n
  show (UniName n i) = n ++ "_" ++ show i
  show (GenName i) = "$" ++ show i

data CompStates
  = Compiling
      { compNextStamp :: Stamp
      }
  | Failed

mkCompStates :: CompStates 
mkCompStates = Compiling { compNextStamp = 0 }

type CompEnvT m a = StateT CompStates m a

type CompEnv a = StateT CompStates Identity a

stamp :: (Monad m) => CompEnvT m Stamp
stamp = do
  s <- get
  let r = compNextStamp s
  put (s {compNextStamp = r + 1})
  pure r

fresh :: (Monad m) => CompEnvT m Name
fresh = GenName <$> stamp

freshStr :: (Monad m) => CompEnvT m String
freshStr = show <$> stamp

freshWithBase :: (Monad m) => String -> CompEnvT m String
freshWithBase s = (s ++) <$> freshStr


uniquify :: (Monad m) => Name -> CompEnvT m Name
uniquify (SynName n) = UniName n <$> stamp
uniquify x = pure x

-- trans :: Int -> CompEnv (State String) ()
-- trans i = do
--         j <- fresh
--         lift $ put (show $ i + j)
