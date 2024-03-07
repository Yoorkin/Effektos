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

subst, append :: Name -> State (Operands,ValCount) Operand
subst n = do mp <- fst <$> get
             case lookup n mp of
               Nothing -> append n
               Just x -> pure x

append n = state $ \(mp,count) -> 
                    let operand = Val count 
                     in (operand, (insert n operand mp, count + 1))


type FnKont = Name


translExpr :: Maybe FnKont -> CPS.Term -> State (Operands, ValCount) [Inst]
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
    mainBlocks = flip evalState  (labelMap . collectFnContNames $ t, 0) $ do
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
collectFnContNames (CPS.LetFns fns _) = concatMap (\(n,CPS.Fn _ _ _ e) -> n : processConts e) fns
       where processConts (CPS.LetConts conts _) = map fst conts 
             processConts _ = []
collectFnContNames _ = []

labelOperandMap :: CPS.Term -> Map Name Operand
labelOperandMap t = fromList $ map f (collectFnContNames t)
              where f n = (n, Label $ show n)


