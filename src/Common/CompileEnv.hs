{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Common.CompileEnv where

import Control.Comonad.Identity
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.State.Lazy (StateT, evalStateT, state)

newtype UUID = UUID Int deriving (Eq, Ord)

instance Show UUID where
  show (UUID i) = "_" ++ show i

data CompCtx
  = Compiling
      { compUuids :: [UUID]
      }

makeCompCtx :: CompCtx
makeCompCtx = Compiling { compUuids = map UUID [0..] }

runComp :: Monad m => CompEnvT m a -> m a
runComp m =  evalStateT m makeCompCtx 

type CompEnvT m a = StateT CompCtx m a

type CompEnv a = StateT CompCtx Identity a


uuid :: (Monad m) => CompEnvT m UUID
uuid = state $ \ctx -> 
         let (uuid':remains) = compUuids ctx
          in (uuid', ctx { compUuids = remains})

hoistIO :: (MFunctor t) => t Identity a -> t IO a
hoistIO = hoist (pure . runIdentity)
