module Common.Name (Name, synName, makeLocalName, makeQualified, toUnique) where

import Common.CompileEnv
import Data.List.Split (splitOn)

data Name
  = Name String
  | QualifiedName [String]
  | LocalName String UUID
  deriving (Show, Eq, Ord)

synName :: String -> Name
synName = Name

makeLocalName :: String -> UUID -> Name
makeLocalName = LocalName

makeQualified :: String -> Name
makeQualified = QualifiedName . splitOn "."

toUnique :: Monad m => Name -> CompEnvT m Name 
toUnique (Name n) = LocalName n <$> uuid
toUnique n@(QualifiedName _) = return n
toUnique n@(LocalName _ _) = return n
