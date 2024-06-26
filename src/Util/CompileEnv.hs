{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Util.CompileEnv
  ( Stamp,
    Name,
    CompStates,
    CompEnvT,
    CompEnv,
    stamp,
    fresh,
    uniName,
    mkCompStates,
    mkCompBeginWith,
    synName,
    freshStr,
    hoistIO,
    baseStr,
  )
where

import Control.Comonad.Identity
import Control.Lens (Plated (..))
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.State.Lazy (StateT, get, put)
import Data.Data
import Data.Data.Lens
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Prettyprinter

type Stamp = Int

data Name
  = SynName String
  | UniName String Stamp
  | GenName Stamp
  deriving (Eq, Ord, Read, Data)

instance Show Name where
  show (SynName n) = n
  show (UniName n i) = n ++ show i
  show (GenName i) = "efk_" ++ show i

instance Plated Name where
  plate = uniplate

instance Pretty Name where
  pretty = pretty . show

data CompStates
  = Compiling
      { compNamedStamp :: Map String Stamp,
        compNextStamp :: Stamp
      }
  | Failed

mkCompStates :: CompStates
mkCompStates = Compiling {compNextStamp = 0, compNamedStamp = Map.empty}

mkCompBeginWith :: Int -> CompStates
mkCompBeginWith x = Compiling {compNextStamp = x, compNamedStamp = Map.empty}

type CompEnvT m a = StateT CompStates m a

type CompEnv a = StateT CompStates Identity a

stamp :: (Monad m) => Maybe String -> CompEnvT m Stamp
stamp mbase =
  case mbase of
    Nothing -> do
      s <- get
      let r = compNextStamp s
      put (s {compNextStamp = r + 1})
      pure r
    Just base -> do
      s <- get
      let mp = compNamedStamp s
      let (r, mp') =
            case Map.lookup base mp of
              Nothing -> (0, Map.insert base 1 mp)
              Just x -> (x, Map.insert base (x + 1) mp)
      put (s {compNamedStamp = mp'})
      pure r

fresh :: (Monad m) => CompEnvT m Name
fresh = GenName <$> stamp Nothing

freshStr :: (Monad m) => String -> StateT CompStates m Name
freshStr str = do
  s <- get
  let mp = compNamedStamp s
  let (r, mp') =
        case Map.lookup str mp of
          Nothing -> (0, Map.insert str 1 mp)
          Just x -> (x, Map.insert str (x + 1) mp)
  put (s {compNamedStamp = mp'})
  pure $ UniName str r

uniName :: (Monad m) => Name -> CompEnvT m Name
uniName (SynName n) = UniName n <$> stamp (Just n)
uniName (UniName n _) = UniName n <$> stamp (Just n)
uniName (GenName _) = GenName <$> stamp Nothing

baseStr :: Name -> String
baseStr (SynName n) = n
baseStr (UniName n _) = n
baseStr (GenName _) = "$"

synName :: String -> Name
synName = SynName

hoistIO :: (MFunctor t) => t Identity a -> t IO a
hoistIO = hoist (pure . runIdentity)
