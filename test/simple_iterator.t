
  $ effektos --show-input --debug-tokens --debug-lambda --debug-uniquify --debug-cps --debug-simplify --debug-closure-conversion --debug-flat --debug-js $TESTDIR/simple_iterator.tos
  =========== Source ================
  let print = fun x -> (extern "console.log" x) in 
  let rec iter = fun x -> let n = raise (Yield,x) in print n; iter n in 
  handle iter 1 with 
  | Yield (x,k) -> if x > 5 then 114514 else resume (k,x + 2) 
  
  =========== Tokens ================
  [ (Symbol "let",1:1,3)
  , (Identifier "print",1:5,5)
  , (Symbol "=",1:11,1)
  , (Symbol "fun",1:13,3)
  , (Identifier "x",1:17,1)
  , (Symbol "->",1:19,2)
  , (Symbol "(",1:22,1)
  , (Symbol "extern",1:23,6)
  , (String "console.log",1:30,13)
  , (Identifier "x",1:44,1)
  , (Symbol ")",1:45,1)
  , (Symbol "in",1:47,2)
  , (Symbol "let",2:1,3)
  , (Symbol "rec",2:5,3)
  , (Identifier "iter",2:9,4)
  , (Symbol "=",2:14,1)
  , (Symbol "fun",2:16,3)
  , (Identifier "x",2:20,1)
  , (Symbol "->",2:22,2)
  , (Symbol "let",2:25,3)
  , (Identifier "n",2:29,1)
  , (Symbol "=",2:31,1)
  , (Symbol "raise",2:33,5)
  , (Symbol "(",2:39,1)
  , (Identifier "Yield",2:40,5)
  , (Symbol ",",2:45,1)
  , (Identifier "x",2:46,1)
  , (Symbol ")",2:47,1)
  , (Symbol "in",2:49,2)
  , (Identifier "print",2:52,5)
  , (Identifier "n",2:58,1)
  , (Symbol ";",2:59,1)
  , (Identifier "iter",2:61,4)
  , (Identifier "n",2:66,1)
  , (Symbol "in",2:68,2)
  , (Symbol "handle",3:1,6)
  , (Identifier "iter",3:8,4)
  , (Number 1,3:13,1)
  , (Symbol "with",3:15,4)
  , (Symbol "|",4:1,1)
  , (Identifier "Yield",4:3,5)
  , (Symbol "(",4:9,1)
  , (Identifier "x",4:10,1)
  , (Symbol ",",4:11,1)
  , (Identifier "k",4:12,1)
  , (Symbol ")",4:13,1)
  , (Symbol "->",4:15,2)
  , (Symbol "if",4:18,2)
  , (Identifier "x",4:21,1)
  , (Symbol ">",4:23,1)
  , (Number 5,4:25,1)
  , (Symbol "then",4:27,4)
  , (Number 114514,4:32,6)
  , (Symbol "else",4:39,4)
  , (Symbol "resume",4:44,6)
  , (Symbol "(",4:51,1)
  , (Identifier "k",4:52,1)
  , (Symbol ",",4:53,1)
  , (Identifier "x",4:54,1)
  , (Symbol "+",4:56,1)
  , (Number 2,4:58,1)
  , (Symbol ")",4:59,1)
  , (Eof,0:0,0)
  ]
  =========== Lambda ================
  Let print
      ( Abs x
          ( PrimOp
              ( Extern "console.log" ) [ Var x ]
          )
      )effektos: src/SyntaxToLambda.hs:(26,7)-(56,47): Non-exhaustive patterns in case
  
  [1]
