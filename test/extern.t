
  $ effektos --show-input --debug-tokens --debug-lambda --debug-uniquify --debug-cps --debug-simplify --debug-closure-conversion --debug-flat --debug-js $TESTDIR/extern.tos
  =========== Source ================
  let print = fun x -> (extern "console.log" x) in 
  print 114514
  
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
  , (Identifier "print",2:1,5)
  , (Number 114514,2:7,6)
  , (Eof,0:0,0)
  ]
  =========== Lambda ================
  Let print
      ( Abs x
          ( PrimOp
              ( Extern "console.log" ) [ Var x ]
          )
      )
      ( App ( Var print )
          ( Const
              ( Integer 114514 )
          )
      )
  =========== Uniquified ================
  Let print (Abs x (PrimOp (Extern "console.log") [Var x])) (App (Var print) (Const (Integer 114514)))
  =========== CPS ================
  Letcont k {} x1 =
      halt x1
  in
  Letval
    f =
      (Fun k1 {} [x] ->
        Letprim r = Extern "console.log" (x) in
        Continue k1 {} r)
  in
  Letcont j {} print =
      Letval c = i32 114514 in
      Letcont k2 {} x2 =
          Continue k {} x2
      in
      print k2 {} [c]
  in
  Continue j {} f
  =========== Closure Passing Style ================
  Letcont code_k {env} x1 =
      halt x1
  in
  Letval k = (code_k) in
  Letval
    code_f =
      (Fun k1 {env1} [x] ->
        Letprim r = Extern "console.log" (x) in
        Letsel k3 = select 0 k1 in
        Continue k3 {k1} r)
  in
  Letval f = (code_f) in
  Letcont code_j {env2} print =
      Letsel k4 = select 1 env2 in
      Letval c = i32 114514 in
      Letcont code_k1 {env3} x2 =
          Letsel k5 = select 1 env3 in
          Letsel k6 = select 0 k5 in
          Continue k6 {k5} x2
      in
      Letval k2 = (code_k1, k4) in
      Letsel $0 = select 0 print in
      $0 k2 {print} [c]
  in
  Letval j = (code_j, k) in
  Letsel k7 = select 0 j in
  Continue k7 {j} f
  =========== Flat ================
  main():
      k = (code_k)
      f = (code_f)
      j = (code_j, k)
      k7 = j[0]
      k7(j, f)
  
  code_k1(env3, x2):
      k5 = env3[1]
      k6 = k5[0]
      k6(k5, x2)
  
  code_j(env2, print):
      k4 = env2[1]
      c = 114514
      k2 = (code_k1, k4)
      $0 = print[0]
      $0(print, k2, c)
  
  code_f(env1, k1, x):
      r = PrimOp (Extern "console.log") [x]
      k3 = k1[0]
      k3(k1, r)
  
  code_k(env, x1):
      exit x1
  
  =========== JS ================
  
    // effect handler
    function _notnull(x){ 
      if(x===undefined){ 
        console.log('undefined') 
      } else {
        return x
      } 
    } 
    const _hds = new Map(); 
    function _push(eff,h){ if(_hds.has(eff)){ _hds.get(eff).push(h) }else{ _hds.set(eff,[h]) } } 
    function _pop(eff){ _hds.get(eff).pop() } 
    function _raise(eff,k,x,resume){ 
      const h = _hds.get(eff).slice(-1)[0]; 
      h[0](h,k,x,resume) 
    } 
    // entry
    main()    
  
  function main(){
    const k = [_notnull(code_k)]
    const f = [_notnull(code_f)]
    const j = [_notnull(code_j),_notnull(k)]
    const k7 = _notnull(j[0])
    k7(j,f)
  }
  function code_k1(env3,x2){
    const k5 = _notnull(env3[1])
    const k6 = _notnull(k5[0])
    k6(k5,x2)
  }
  function code_j(env2,print){
    const k4 = _notnull(env2[1])
    const c = 114514
    const k2 = [_notnull(code_k1),_notnull(k4)]
    const $0 = _notnull(print[0])
    $0(print,k2,c)
  }
  function code_f(env1,k1,x){
    const r = console.log(x)
    const k3 = _notnull(k1[0])
    k3(k1,r)
  }
  function code_k(env,x1){
  
    console.log(x1)
  }
