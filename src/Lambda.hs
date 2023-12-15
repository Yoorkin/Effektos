module Lambda where

type Fn = (String,Expr) 

data Constant 
	= Integer Int
	deriving Show

data Primitive
 = Add2 | Sub2 | Abort
 deriving Show

data Represent 
	= TaggedRep Int
	deriving Show

data Constructor 
	= DataCon Represent 
	| ConstCon Constant 
	deriving Show

data Expr 
	= Var String
	| Abs String Expr
	| Let String Expr Expr
	| App Expr Expr
	| Fix [Fn] Expr
	| Const Constant
	| Tuple [Expr]
	| Select Int Expr
	| PrimOp Primitive [Expr]
	| Constr Represent Expr
	| Decon Represent Expr
	| Switch Expr [Int] (Maybe Expr)
	deriving Show
