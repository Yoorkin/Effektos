{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ASM.X64 where

import ASM.RTL
import qualified ASM.RTL as RTL
import Data.List

getByteReg :: Operand -> String
getByteReg (Reg i) = 
  [ "%bl",
    "%cl",
    "%dl",
    "%r8b",
    "%r9b",
    "%r10b",
    "%r11b",
    "%r12b",
    "%r13b",
    "%r14b",
    "%r15b"
  ]
    !! i
getByteReg RLK = "%al"
getByteReg _ = ""

translOperand :: Operand -> String
translOperand (Reg i) =
  [ "%rbx",
    "%rcx",
    "%rdx",
    "%r8",
    "%r9",
    "%r10",
    "%r11",
    "%r12",
    "%r13",
    "%r14",
    "%r15"
  ]
    !! i
translOperand RLK = "%rax"
translOperand (Val _) = error "invalid operand"
translOperand (Label l) = "$" ++ label l
translOperand (Foreign name) = name
translOperand (I64 i) = "$" ++ show i
translOperand Unit = "$0"
translOperand _ = error ""

(+-+) :: String -> String -> String
a +-+ b = a ++ " " ++ b

emit :: String -> [Operand] -> String
emit base operands = indent ++ base ++ " " ++ intercalate ", " operands'
  where
    operands' = map translOperand operands

label :: Label -> String
label l = l

indent :: String
indent = "    "

indirect :: Operand -> String
indirect operand =
  case operand of
    Label l -> l
    Reg _ -> "*" ++ operand'
    RLK -> "*" ++ operand'
    _ -> operand'
  where
    operand' = translOperand operand

isImm :: Operand -> Bool
isImm (I64 _) = True
isImm Unit = True
isImm _ = False

comment :: String -> String
comment x = "//" ++ x

translInstr :: Inst -> [String]
translInstr (Binary a op b c) =
  case op of
    RTL.EQ ->
      [ emit "cmpq" (if isImm b then [b, c] else [c, b]),
        indent ++ "sete " ++ getByteReg a
      ]
    Add2 ->
      [ emit "movq" [b, a],
        emit "addq" [c, a]
      ]
    Sub2 ->
      [ emit "movq" [b, a],
        emit "subq" [c, a]
      ]
translInstr (Unary a op b) =
  case op of
    Add1 -> [emit "movq" [b, a]]
    Sub1 ->
      [ emit "movq" [b, a],
        emit "negq" [a]
      ]
translInstr (Move a b) = [emit "movq" [b, a]]
translInstr (Load dst src ofs) =
  [indent ++ "movq" +-+ src' ++ ", " ++ translOperand dst]
  where
    src' =
      case ofs of
        (I64 i)
          | i == 0 -> translOperand src
          | otherwise -> show i ++ "(" ++ translOperand src ++ ")"
        (Reg _) -> "(" ++ translOperand src ++ "," ++ translOperand ofs ++ ")"
        RLK -> "(" ++ translOperand src ++ "," ++ translOperand ofs ++ ")"
        _ -> error ""
translInstr (Store dst src ofs) =
  [indent ++ "movq" +-+ src' ++ ", " ++ translOperand dst]
  where
    src' =
      case ofs of
        (I64 i)
          | i == 0 -> translOperand src
          | otherwise -> show i ++ "(" ++ translOperand src ++ ")"
        (Reg _) -> "(" ++ translOperand src ++ "," ++ translOperand ofs ++ ")"
        RLK -> "(" ++ translOperand src ++ "," ++ translOperand ofs ++ ")"
        _ -> error ""
translInstr (Goto a) = [indent ++ "jmp " ++ indirect a]
translInstr (Jeq a b c)
  | Label _ <- c =
      [ emit "cmpq" (if isImm a then [a, b] else [b, a]),
        indent ++ "je " ++ indirect c
      ]
  | otherwise = error "invalid jeq"
translInstr (Call a _) = [indent ++ "call " ++ indirect a]
translInstr (NewBlock l) =
  case l of
    "start" -> []
    _ -> [label l ++ ":"]
translInstr Ret = [emit "ret" []]

translate :: Program -> String
translate (Program fns instr) = main ++ fns'
  where
    main = hsep (label "main" ++ ":") (concatMap translInstr instr)
    fns' = hsep "" $ map translFn fns
    translFn (Fn str fnInstr) = hsep (label str ++ ":    " ++ comment "function") (concatMap translInstr fnInstr)
    hsep = foldl (\x y -> x ++ "\n" ++ y)
