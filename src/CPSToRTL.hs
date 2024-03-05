module CPSToRTL where

import qualified CPS
import CompileEnv
import Control.Monad (zipWithM)
import Control.Monad.Extra (concatMapM)
import Control.Monad.State.Lazy
import Data.Map.Lazy hiding (map)
import qualified Data.Map.Lazy as Map
import Control.Monad.Morph (hoist)
import Data.Maybe (fromJust)
import GHC.Num (integerFromInt)
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
subst n = do r <- lookup n <$> lift get
             case r of
                Nothing -> append n 
                Just x -> pure x

append :: Name -> CompEnvT (State OperandMap) Operand
append n = do 
  operand <- Val <$> stamp Nothing
  lift (modify $ insert n operand)
  return operand

translExpr :: Maybe FnKont -> CPS.Term -> CompEnvT (State OperandMap) [Inst]
translExpr fnk term =
  let go = translExpr fnk
   in case term of
        (CPS.LetVal n l t) -> do
          v <- append n
          inst <-
            case l of
              (CPS.Var n') -> do
                v2 <- subst n'
                pure [Move v v2]
              (CPS.I32 i) -> pure [Move v (I64 $ integerFromInt i)]
              CPS.Unit -> pure [Move v Unit]
              CPS.Tuple xs -> do
                moveInsts <-
                  zipWithM
                    ( \i x -> do
                        x' <- subst x
                        pure (Store v x' (I64 $ i * 8))
                    )
                    [0 ..]
                    xs
                pure
                  ( [ Move (Reg 1) (I64 $ integerFromInt (length xs)),
                      Call "malloc",
                      Move v (Reg 1)
                    ]
                      ++ moveInsts
                  )
          insts <- go t
          pure (inst ++ insts)
        (CPS.LetSel n1 i n2 t) -> do
          v <- append n1
          n2' <- subst n2
          let inst = Load v n2' (I64 $ integerFromInt $ i * 8)
          insts <- go t
          pure (inst : insts)
        (CPS.LetCont {}) -> error "invalid input"
        (CPS.LetConts {}) -> error "invalid input"
        (CPS.LetFns {}) -> error "invalid input"
        (CPS.Continue k x)
          | Just k' <- fnk,
            k' == k -> do
              x' <- subst x
              pure [Move (Reg 1) x', Ret]
          | otherwise -> do
              x' <- subst x
              pure [Move (Reg 1) x', Goto (Label (show k))]
        (CPS.Apply f k env xs) -> do
          argsMovInst <-
            zipWithM
              ( \i x -> do
                  x' <- subst x
                  pure (Move (Reg i) x')
              )
              [1 ..]
              xs
          k' <- subst k
          f' <- subst f
          pure
            ( argsMovInst
                ++ [ Move RLK k',
                     Goto f'
                   ]
            )
        (CPS.LetPrim n op xs t) -> pure []
        (CPS.Switch n cs ts fb) -> pure []
        (CPS.Halt x) -> do x' <- subst x
                           pure [Move (Reg 1) x', Call "exit"]

translCont :: Maybe FnKont -> (Name, CPS.Value) -> CompEnvT (State OperandMap) BasicBlock
translCont fnk (n, CPS.Cont x t) = do
  v <- append x
  let inst = Move v (Reg 1)
  insts <- translExpr fnk t
  return $ BasicBlock (show n) (inst : insts)
translCont _ _ = error "invalid input"

translFn :: (Name, CPS.Value) -> CompEnvT (State OperandMap) Fn
translFn (n, CPS.Fn k (Just env) xs t) = do
  let (conts, t') =
        case t of
          (CPS.LetConts cs m) -> (cs, m)
          _ -> ([], t)
  blocks <- mapM (translCont (Just k)) conts
  inst <- zipWithM (\i x -> Move <$> append x <*> pure (Reg i)) [1 ..] (env : xs)
  insts <- translExpr Nothing t'
  let entry = BasicBlock "start" (inst ++ insts)
  return $ Fn (show n) (entry : blocks)
translFn _ = error "invalid input"

translate' :: CPS.Term -> CompEnvT (State OperandMap) Program
translate' t = do
  let (fns, t1) =
        case t of
          (CPS.LetFns fs m) -> (fs, m)
          x -> ([], x)
  fns' <- mapM translFn fns
  let (conts, t2) =
        case t1 of
          (CPS.LetConts cs m) -> (cs, m)
          x -> ([], x)
  blocks <- mapM (translCont Nothing) conts
  entry <- BasicBlock "start" <$> translExpr Nothing t2
  pure $ Program fns' (entry : blocks)


translate :: CPS.Term -> CompEnv Program
translate t = hoist (`evalStateT` Map.empty) (translate' t)
