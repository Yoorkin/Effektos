
  $ effektos --show-input --debug-tokens --debug-lambda --debug-uniquify --debug-cps --debug-simplify --debug-closure-conversion --debug-flat --debug-js $TESTDIR/function_apply.tos
  =========== Source ================
  
  let n = 5 + 2 in let f = fun x -> x + n in f (f 114)
  
  =========== Tokens ================
  [ (Symbol "let",2:1,3)
  , (Identifier "n",2:5,1)
  , (Symbol "=",2:7,1)
  , (Number 5,2:9,1)
  , (Symbol "+",2:11,1)
  , (Number 2,2:13,1)
  , (Symbol "in",2:15,2)
  , (Symbol "let",2:18,3)
  , (Identifier "f",2:22,1)
  , (Symbol "=",2:24,1)
  , (Symbol "fun",2:26,3)
  , (Identifier "x",2:30,1)
  , (Symbol "->",2:32,2)
  , (Identifier "x",2:35,1)
  , (Symbol "+",2:37,1)
  , (Identifier "n",2:39,1)
  , (Symbol "in",2:41,2)
  , (Identifier "f",2:44,1)
  , (Symbol "(",2:46,1)
  , (Identifier "f",2:47,1)
  , (Number 114,2:49,3)
  , (Symbol ")",2:52,1)
  , (Eof,0:0,0)
  ]
  =========== Lambda ================
  Let n
      ( PrimOp Add2
          [ Const
              ( Integer 5 )
          , Const
              ( Integer 2 )
          ]
      )
      ( Let f
          ( Abs x
              ( PrimOp Add2
                  [ Var x
                  , Var n
                  ]
              )
          )
          ( App ( Var f )
              ( App ( Var f )
                  ( Const
                      ( Integer 114 )
                  )
              )
          )
      )
  =========== Uniquified ================
  Let n (PrimOp Add2 [Const (Integer 5),Const (Integer 2)]) (Let f (Abs x (PrimOp Add2 [Var x,Var n])) (App (Var f) (App (Var f) (Const (Integer 114)))))
  =========== CPS ================
  Letcont k {} x1 =
      halt x1
  in
  Letval c = i32 5 in
  Letval c1 = i32 2 in
  Letprim r = Add2 (c, c1) in
  Letcont j {} n =
      Letval
        f1 =
          (Fun k1 {} [x] ->
            Letprim r1 = Add2 (x, n) in
            Continue k1 {} r1)
      in
      Letcont j1 {} f =
          Letval c2 = i32 114 in
          Letcont k3 {} x3 =
              Letcont k2 {} x2 =
                  Continue k {} x2
              in
              f k2 {} [x3]
          in
          f k3 {} [c2]
      in
      Continue j1 {} f1
  in
  Continue j {} r
  =========== Closure Passing Style ================
  Letcont code_k {env} x1 =
      halt x1
  in
  Letval k = (code_k) in
  Letval c = i32 5 in
  Letval c1 = i32 2 in
  Letprim r = Add2 (c, c1) in
  Letcont code_j {env1} n =
      Letsel k4 = select 1 env1 in
      Letval
        code_f =
          (Fun k1 {env2} [x] ->
            Letsel n1 = select 1 env2 in
            Letprim r1 = Add2 (x, n1) in
            Letsel k5 = select 0 k1 in
            Continue k5 {k1} r1)
      in
      Letval f1 = (code_f, n) in
      Letcont code_j1 {env3} f =
          Letsel k6 = select 1 env3 in
          Letval c2 = i32 114 in
          Letcont code_k1 {env4} x3 =
              Letsel f2 = select 2 env4 in
              Letsel k7 = select 1 env4 in
              Letcont code_k2 {env5} x2 =
                  Letsel k8 = select 1 env5 in
                  Letsel k9 = select 0 k8 in
                  Continue k9 {k8} x2
              in
              Letval k2 = (code_k2, k7) in
              Letsel $0 = select 0 f2 in
              $0 k2 {f2} [x3]
          in
          Letval k3 = (code_k1, k6, f) in
          Letsel $1 = select 0 f in
          $1 k3 {f} [c2]
      in
      Letval j1 = (code_j1, k4) in
      Letsel k10 = select 0 j1 in
      Continue k10 {j1} f1
  in
  Letval j = (code_j, k) in
  Letsel k11 = select 0 j in
  Continue k11 {j} r
  =========== Flat ================
  main():
      k = (code_k)
      c = 5
      c1 = 2
      r = PrimOp Add2 [c,c1]
      j = (code_j, k)
      k11 = j[0]
      k11(j, r)
  
  code_k2(env5, x2):
      k8 = env5[1]
      k9 = k8[0]
      k9(k8, x2)
  
  code_k1(env4, x3):
      f2 = env4[2]
      k7 = env4[1]
      k2 = (code_k2, k7)
      $0 = f2[0]
      $0(f2, k2, x3)
  
  code_j1(env3, f):
      k6 = env3[1]
      c2 = 114
      k3 = (code_k1, k6, f)
      $1 = f[0]
      $1(f, k3, c2)
  
  code_f(env2, k1, x):
      n1 = env2[1]
      r1 = PrimOp Add2 [x,n1]
      k5 = k1[0]
      k5(k1, r1)
  
  code_j(env1, n):
      k4 = env1[1]
      f1 = (code_f, n)
      j1 = (code_j1, k4)
      k10 = j1[0]
      k10(j1, f1)
  
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
    const c = 5
    const c1 = 2
    const r = c + c1
    const j = [_notnull(code_j),_notnull(k)]
    const k11 = _notnull(j[0])
    k11(j,r)
  }
  function code_k2(env5,x2){
    const k8 = _notnull(env5[1])
    const k9 = _notnull(k8[0])
    k9(k8,x2)
  }
  function code_k1(env4,x3){
    const f2 = _notnull(env4[2])
    const k7 = _notnull(env4[1])
    const k2 = [_notnull(code_k2),_notnull(k7)]
    const $0 = _notnull(f2[0])
    $0(f2,k2,x3)
  }
  function code_j1(env3,f){
    const k6 = _notnull(env3[1])
    const c2 = 114
    const k3 = [_notnull(code_k1),_notnull(k6),_notnull(f)]
    const $1 = _notnull(f[0])
    $1(f,k3,c2)
  }
  function code_f(env2,k1,x){
    const n1 = _notnull(env2[1])
    const r1 = x + n1
    const k5 = _notnull(k1[0])
    k5(k1,r1)
  }
  function code_j(env1,n){
    const k4 = _notnull(env1[1])
    const f1 = [_notnull(code_f),_notnull(n)]
    const j1 = [_notnull(code_j1),_notnull(k4)]
    const k10 = _notnull(j1[0])
    k10(j1,f1)
  }
  function code_k(env,x1){
  
    console.log(x1)
  }
