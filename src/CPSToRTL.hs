{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CPSToRTL where

import qualified CPS
import CompileEnv
import qualified Constant
import Control.Monad (mapAndUnzipM, zipWithM)
import Control.Monad.Extra (concatMapM)
import qualified Control.Monad.State.Lazy as State
import Data.Map.Lazy hiding (map)
import GHC.Num (integerFromInt)
import qualified Primitive as Op
import RTL
import Prelude hiding (lookup)

type Operands = Map Name Operand

type ValCount = Int

type LabelCount = Int

type State a = State.State (Operands, LabelCount, ValCount) a

subst, append :: Name -> State Operand
subst n = do
  (mp, _, _) <- State.get
  case lookup n mp of
    Nothing -> append n
    Just x -> pure x
append n = State.state $ \(mp, label, count) ->
  let operand = Val count
   in (operand, (insert n operand mp, label, count + 1))

freshLabel :: State Label
freshLabel = State.state $ \(a, label, c) -> ("block" ++ show label, (a, label + 1, c))

type BasicBlocks = [BasicBlock]

type FnKont = Name

translValue :: Name -> CPS.Value -> State [Inst]
translValue n1 (CPS.Var n2) = do
  n1' <- subst n1
  n2' <- subst n2
  return [Move n1' n2']
translValue n (CPS.I32 i) = do
  n' <- subst n
  return [Move n' value]
  where
    value = I64 $ integerFromInt i
translValue n CPS.Unit = do
  n' <- subst n
  return [Move n' Unit]
translValue n (CPS.Tuple xs) =
  (++) <$> mallocInsts <*> movInsts
  where
    mallocInsts = do
      v <- subst n
      label <- freshLabel
      pure
        [ Move (Reg 1) (I64 $ integerFromInt (length xs)),
          Move RLK (Label label),
          Call "malloc",
          NewBlock label,
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

translExpr :: Maybe FnKont -> CPS.Term -> State [Inst]
translExpr fnk (CPS.LetVal n l t) = do
  valueInsts <- translValue n l
  remainInsts <- translExpr fnk t
  return (valueInsts ++ remainInsts)
translExpr fnk (CPS.LetSel n1 i n2 t) = do
  n1' <- subst n1
  n2' <- subst n2
  insts <- translExpr fnk t
  return (Load n1' n2' index : insts)
  where
    index = I64 (integerFromInt (i * 8))
translExpr _ (CPS.LetCont {}) = error "invalid input"
translExpr _ (CPS.LetConts {}) = error "invalid input"
translExpr _ (CPS.LetFns {}) = error "invalid input"
translExpr fnk (CPS.Continue k x)
  | Just k' <- fnk,
    k' == k = do
      x' <- subst x
      return
        [ Move (Reg 1) x',
          Ret
        ]
  | otherwise = do
      x' <- subst x
      return
        [ Move (Reg 1) x',
          Goto (Label (show k))
        ]
translExpr _ (CPS.Apply _ _ Nothing _) = undefined
translExpr _ (CPS.Apply f k (Just env) xs) = do
  (++) <$> argsMovInst <*> callInst
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
translExpr fnk (CPS.LetPrim n op args t) = do
  inst <- pmt op args
  insts <- translExpr fnk t
  return (inst : insts)
  where
    binary o a b = Binary <$> subst n <*> pure o <*> subst a <*> subst b
    unary o a = Unary <$> subst n <*> pure o <*> subst a
    pmt Op.Add2 [a, b] = binary Add2 a b
    pmt Op.Sub2 [a, b] = binary Sub2 a b
    pmt Op.EQ [a, b] = binary RTL.EQ a b
    pmt Op.Sub1 [a] = unary Sub1 a
    pmt a b = error $ show (a, b)
translExpr fnk (CPS.Switch n cs ts fb) = do
  (testInstr, caseInstr) <- mapAndUnzipM translCase (zip cs ts)
  fallbackInstr <- getFallbackInstr
  return (testInstr ++ fallbackInstr ++ concat caseInstr)
  where
    translConst = \case
      (Constant.Integer i) -> (I64 $ integerFromInt i)
      Constant.Unit -> Unit
    translCase (const, term) = do
      n' <- subst n
      caseLabel <- freshLabel
      caseInstr <- translExpr fnk term
      let testInstr = Jeq n' (translConst const) (Label caseLabel)
      let caseBlock = NewBlock caseLabel : caseInstr
      return (testInstr, caseBlock)
    getFallbackInstr = do
      case fb of
        Nothing -> return [] -- should be error
        Just fb' -> translExpr fnk fb'
translExpr _ (CPS.Halt x) = do
  x' <- subst x
  return
    [ Move (Reg 1) x',
      Call "exit"
    ]

translCont :: Maybe FnKont -> (Name, CPS.Value) -> State [Inst]
translCont fnk (n, CPS.Cont x t) = do
  inst <- Move <$> append x <*> pure (Reg 1)
  insts <- translExpr fnk t
  return
    ( NewBlock (show n)
        : inst
        : insts
    )
translCont _ _ = error "invalid input"

labelMap :: (Ord a, Show a) => [a] -> Map a Operand
labelMap names = fromList (map (\n -> (n, Label $ show n)) names)

translFn :: [Name] -> (Name, CPS.Value) -> Fn
translFn contFnNames (n, CPS.Fn k (Just env) xs t) = Fn (show n) [entryBlock]
  where
    initState = (labelMap contFnNames, 0, 0)
    entryBlock = flip State.evalState initState $ do
      contInst <- concatMapM (translCont (Just k)) conts
      argSavingInst <- zipWithM (\i x -> Move <$> append x <*> pure (Reg i)) [1 ..] (env : xs)
      entryInst <- translExpr (Just k) t'
      return $ BasicBlock "start" (NewBlock "start" : argSavingInst ++ entryInst ++ contInst)
    (conts, t') =
      case t of
        (CPS.LetConts cs m) -> (cs, m)
        _ -> ([], t)
translFn _ _ = error "invalid input"

translate :: CPS.Term -> Program
translate t = Program fns' mainBlocks
  where
    mainBlocks = flip State.evalState (labelMap . collectFnContNames $ t, 0, 0) $ do
      contInstr <- concatMapM (translCont Nothing) conts
      entryInstr <- translExpr Nothing t2
      let instr = NewBlock "start" : entryInstr ++ contInstr
      return [BasicBlock "start" instr]
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
