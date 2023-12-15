module CPS where
import qualified Lambda as L

type Name = String

data Value
  = Var Name
	| I32 Int
	| Unit
	| Tuple [Value]
	| Cont Name Term    -- k e
  | Fn Name Name Term -- k x e
	deriving Show

data Term
  = LetVal Name Value Term
	| LetSel Name Int Value Term
	| LetCont Name Name Term Term
	| Continue Value Value
	| Apply Value Value Value
  | LetPrim Name L.Primitive [Value] Term
	| Switch Value [Term]
	| Halt Value
	deriving Show

