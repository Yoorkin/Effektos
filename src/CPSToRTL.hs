module CPSToRTL where

import qualified CPS
import CompileEnv
import Control.Monad.State.Lazy
import Data.Map.Lazy hiding (map)
import RTL
import Prelude hiding (lookup)

-- translate :: Term -> Program
-- translate

-- freshVal :: CompEnv Operand
-- freshVal = Val stamp

type OperandMap = Map Name Operand

type ContArgMap = Map Name Operand

type FnKont = Name

subst :: Name -> CompEnvT (State OperandMap) Operand
append :: Name -> CompEnvT (State OperandMap) Operand
translExpr :: Maybe FnKont -> CPS.Term -> CompEnvT (State OperandMap) [Inst]
translExpr fnk term =
  let go = translExpr
   in case term of
        (CPS.LetVal n l t) -> do
          v <- append n
          let inst =
                case l of
                  (CPS.Var n') -> [Move v (subst n')]
                  (CPS.I32 i) -> [Move v (I64 i)]
                  CPS.Unit -> [Move v Unit]
                  CPS.Tuple xs ->
                    [ Move (Reg 1) (I64 (length xs)),
                      Invoke "malloc",
                      Move v (reg 1)
                    ]
                      ++ concatMap (\(i, x) -> [Store v (subst x) (I64 $ i * 8)]) xs
          insts <- go t
          pure (inst : insts)
        (CPS.LetSel n1 i n2 t) -> do
          v <- append n1
          let inst = Load v (subst n2) (I64 i * 8)
          insts <- go t
          pure (inst : insts)
        (CPS.LetCont {}) -> error "invalid input"
        (CPS.LetConts {}) -> error "invalid input"
        (CPS.LetFns {}) -> error "invalid input"
        (CPS.Continue k x) 
          | Just k' <- fnk, k' == k ->
              pure [ Move (Reg 1) (subst x), Ret]
          | otherwise -> 
              pure [ Move (Reg 1) (subst x), Goto (Label (show k))]
        (CPS.Apply f k env xs) -> do
          pure $
            zipWith (\i x -> Move (Reg i) (subst x)) [1 ..] xs
              ++ [ Move RLK (subst k),
                   Goto (subst f)
                 ]
        (CPS.LetPrim n op xs t) -> error ""
        (CPS.Switch n cs ts fb) -> error ""

translCont :: Maybe FnKont -> (Name, CPS.Value) -> CompEnvT a BasicBlock
translCont fnk (n, CPS.Cont x t) = do
    v <- append x
    let inst = Move v (Reg 1)
    insts <- translExpr fnk t
    BasicBlock (show n) (inst : insts)
translCont _ = error "invalid input"

translFn :: (Name, CPS.Value) -> CompEnvT a BasicBlock
translFn (n, Fn k (Just env) xs t) = do
    let (conts, t') = 
            case t of
              (CPS.LetConts conts m) -> (conts, m)
              _ -> ([], t)
    blocks <- mapM (translCont (Just k)) conts
    entry <- BasicBlock "start" <$> translExpr Nothing t
    pure $ Fn (show n) (entry:blocks)
translFn _ = error "invalid input"

translate :: CPS.Term -> CompEnvT a Program
translate t = do
  let (fns, t1) = 
        case t of
          (CPS.LetFns fns m) -> (fns,m)
          x -> ([],x)
  fns' <- mapM translFn fns
  let (conts, t2) =
        case t1 of
          (CPS.LetConts conts m) -> (conts, m)
          x -> ([],x)
  blocks <- mapM (translCont (Just k)) conts
  entry <- BasicBlock "start" <$> translExpr Nothing t
  pure $ Program fns' (entry:blocks)


