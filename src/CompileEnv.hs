{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CompileEnv
  ( Stamp,
    Name,
    CompStates,
    CompEnvT,
    CompEnv,
    stamp,
    fresh,
    uniName,
    mkCompStates,
    synName,
    freshStr,
    hoistIO
  )
where

import Control.Comonad.Identity
import Control.Monad.State.Lazy (StateT, get, put)
import Data.Data
import Prettyprinter
import Control.Lens ( Plated(..) )
import Data.Data.Lens
import Control.Monad.Morph (hoist, MFunctor)

type Stamp = Int

data Name
  = SynName String
  | UniName String Stamp
  | GenName Stamp
  deriving (Eq, Ord, Read, Data)

instance Show Name where
  show (SynName n) = n
  show (UniName n i) = n ++ show i
  show (GenName i) = "$" ++ show i

instance Plated Name where
  plate = uniplate

instance Pretty Name where
  pretty = pretty . show

data CompStates
  = Compiling
      { compNextStamp :: Stamp
      }
  | Failed

mkCompStates :: CompStates
mkCompStates = Compiling {compNextStamp = 0}

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

freshStr :: Monad m => String -> StateT CompStates m Name
freshStr s = UniName s <$> stamp

uniName :: (Monad m) => Name -> CompEnvT m Name
uniName (SynName n) = UniName n <$> stamp
uniName (UniName n _) = UniName n <$> stamp
uniName (GenName _) = GenName <$> stamp

synName :: String -> Name
synName = SynName

hoistIO :: (MFunctor t) => t Identity a -> t IO a
hoistIO = hoist (pure . runIdentity)
