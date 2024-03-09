{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CPSToRTL where

import qualified CPS
import CompileEnv
import Control.Monad (zipWithM)
import Control.Monad.State.Lazy
import Data.Map.Lazy hiding (map)
import GHC.Num (integerFromInt)
import RTL
import Prelude hiding (lookup)

type Operands = Map Name Operand

type ValCount = Int

subst, append :: Name -> State (Operands, ValCount) Operand
subst n = do
  mp <- fst <$> get
  case lookup n mp of
    Nothing -> append n
    Just x -> pure x
append n = state $ \(mp, count) ->
  let operand = Val count
   in (operand, (insert n operand mp, count + 1))

type FnKont = Name

translValue :: Name -> CPS.Value -> State (Operands, ValCount) [Inst]
translValue n1 (CPS.Var n2) = do
  mov <- Move <$> subst n1 <*> subst n2
  pure [mov]
translValue n (CPS.I32 i) = do
  mov <- Move <$> subst n <*> pure (I64 $ integerFromInt i)
  pure [mov]
translValue n CPS.Unit = do
  inst <- Move <$> subst n <*> pure Unit
  pure [inst]
translValue n (CPS.Tuple xs) = (++) <$> mallocInsts <*> movInsts
  where
    mallocInsts = do
      v <- subst n
      pure
        [ Move (Reg 1) (I64 $ integerFromInt (length xs)),
          Call "malloc",
          Move v (Reg 1)
        ]
    movInsts =
      zipWithM
        ( \i x ->
            Store <$> subst n <*> subst x <*> pure (I64 (i * 8))
        )
        [0 ..]
        xs
translValue _ _ = undefined

translExpr :: Maybe FnKont -> CPS.Term -> State (Operands, ValCount) [Inst]
translExpr fnk (CPS.LetVal n l t) = (++) <$> translValue n l <*> translExpr fnk t
translExpr fnk (CPS.LetSel n1 i n2 t) = (:) <$> inst <*> insts
  where
    inst = Load <$> subst n1 <*> subst n2 <*> pure (I64 $ integerFromInt $ i * 8)
    insts = translExpr fnk t
translExpr _ (CPS.LetCont {}) = error "invalid input"
translExpr _ (CPS.LetConts {}) = error "invalid input"
translExpr _ (CPS.LetFns {}) = error "invalid input"
translExpr fnk (CPS.Continue k x)
  | Just k' <- fnk,
    k' == k = do
      x' <- subst x
      pure [Move (Reg 1) x', Ret]
  | otherwise = do
      x' <- subst x
      pure [Move (Reg 1) x', Goto (Label (show k))]
translExpr _ (CPS.Apply _ _ Nothing _) = undefined
translExpr _ (CPS.Apply f k (Just env) xs) = (++) <$> argsMovInst <*> callInst
  where
    argsMovInst =
      zipWithM
        ( \i x ->
            Move (Reg i) <$> subst x
        )
        [1 ..]
        (env : xs)
    callInst = do
      k' <- subst k
      f' <- subst f
      pure
        [ Move RLK k',
          Goto f'
        ]
translExpr _ (CPS.LetPrim n op xs t) = undefined
translExpr _ (CPS.Switch n cs ts fb) = undefined
translExpr _ (CPS.Halt x) = do
  x' <- subst x
  pure [Move (Reg 1) x', Call "exit"]

translCont :: Maybe FnKont -> (Name, CPS.Value) -> State (Operands, ValCount) BasicBlock
translCont fnk (n, CPS.Cont x t) = do
  v <- append x
  let inst = Move v (Reg 1)
  insts <- translExpr fnk t
  return $ BasicBlock (show n) (inst : insts)
translCont _ _ = error "invalid input"

labelMap :: (Ord a, Show a) => [a] -> Map a Operand
labelMap names = fromList (map (\n -> (n, Label $ show n)) names)

translFn :: [Name] -> (Name, CPS.Value) -> Fn
translFn contFnNames (n, CPS.Fn k (Just env) xs t) = Fn (show n) allBlocks
  where
    initState = (labelMap contFnNames, 0)
    allBlocks = flip evalState initState $ do
      blocks <- mapM (translCont (Just k)) conts
      inst <- zipWithM (\i x -> Move <$> append x <*> pure (Reg i)) [1 ..] (env : xs)
      insts <- translExpr (Just k) t'
      let entry = BasicBlock "start" (inst ++ insts)
      pure (entry : blocks)
    (conts, t') =
      case t of
        (CPS.LetConts cs m) -> (cs, m)
        _ -> ([], t)
translFn _ _ = error "invalid input"

translate :: CPS.Term -> Program
translate t = Program fns' mainBlocks
  where
    mainBlocks = flip evalState (labelMap . collectFnContNames $ t, 0) $ do
      blocks <- mapM (translCont Nothing) conts
      expr <- translExpr Nothing t2
      pure (BasicBlock "start" expr : blocks)
    fns' = map (translFn labels) fns
    labels = collectFnContNames t
    (fns, t1) =
      case t of
        (CPS.LetFns fs m) -> (fs, m)
        x -> ([], x)
    (conts, t2) =
      case t1 of
        (CPS.LetConts cs m) -> (cs, m)
        x -> ([], x)

collectFnContNames :: CPS.Term -> [Name]
collectFnContNames (CPS.LetFns fns _) = concatMap (\(n, CPS.Fn _ _ _ e) -> n : processConts e) fns
  where
    processConts (CPS.LetConts conts _) = map fst conts
    processConts _ = []
collectFnContNames _ = []

labelOperandMap :: CPS.Term -> Map Name Operand
labelOperandMap t = fromList $ map f (collectFnContNames t)
  where
    f n = (n, Label $ show n)
