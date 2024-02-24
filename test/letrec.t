
  $ effektos --show-input --debug-tokens --debug-lambda --debug-uniquify --debug-cps --debug-simplify --debug-closure-conversion --debug-flat --debug-js $TESTDIR/letrec.tos
  =========== Source ================
  let y = 2 in let rec f = fun x -> g (x + y) 
  and g = fun x -> if 4 > x then f (x + y) else x in
  f 1
  
  =========== Tokens ================
  [ (Symbol "let",1:1,3)
  , (Identifier "y",1:5,1)
  , (Symbol "=",1:7,1)
  , (Number 2,1:9,1)
  , (Symbol "in",1:11,2)
  , (Symbol "let",1:14,3)
  , (Symbol "rec",1:18,3)
  , (Identifier "f",1:22,1)
  , (Symbol "=",1:24,1)
  , (Symbol "fun",1:26,3)
  , (Identifier "x",1:30,1)
  , (Symbol "->",1:32,2)
  , (Identifier "g",1:35,1)
  , (Symbol "(",1:37,1)
  , (Identifier "x",1:38,1)
  , (Symbol "+",1:40,1)
  , (Identifier "y",1:42,1)
  , (Symbol ")",1:43,1)
  , (Symbol "and",2:1,3)
  , (Identifier "g",2:5,1)
  , (Symbol "=",2:7,1)
  , (Symbol "fun",2:9,3)
  , (Identifier "x",2:13,1)
  , (Symbol "->",2:15,2)
  , (Symbol "if",2:18,2)
  , (Number 4,2:21,1)
  , (Symbol ">",2:23,1)
  , (Identifier "x",2:25,1)
  , (Symbol "then",2:27,4)
  , (Identifier "f",2:32,1)
  , (Symbol "(",2:34,1)
  , (Identifier "x",2:35,1)
  , (Symbol "+",2:37,1)
  , (Identifier "y",2:39,1)
  , (Symbol ")",2:40,1)
  , (Symbol "else",2:42,4)
  , (Identifier "x",2:47,1)
  , (Symbol "in",2:49,2)
  , (Identifier "f",3:1,1)
  , (Number 1,3:3,1)
  , (Eof,0:0,0)
  ]
  =========== Lambda ================
  Let y
      ( Const
          ( Integer 2 )
      )
      ( Fix
          [ f
          , g
          ]
          [
              ( x
              , App ( Var g )
                  ( PrimOp Add2
                      [ Var x
                      , Var y
                      ]
                  )
              )
          ,
              ( x
              , Switch
                  ( PrimOp GT
                      [ Const
                          ( Integer 4 )
                      , Var x
                      ]
                  )
                  [
                      ( 1
                      , App ( Var f )
                          ( PrimOp Add2
                              [ Var x
                              , Var y
                              ]
                          )
                      )
                  ,
                      ( 0
                      , Var x
                      )
                  ] Nothing
              )
          ]
          ( App ( Var f )
              ( Const
                  ( Integer 1 )
              )
          )
      )
  =========== Uniquified ================
  Let y (Const (Integer 2)) (Fix [f,g] [(x,App (Var g) (PrimOp Add2 [Var x,Var y])),(x1,Switch (PrimOp GT [Const (Integer 4),Var x1]) [(1,App (Var f) (PrimOp Add2 [Var x1,Var y])),(0,Var x1)] Nothing)] (App (Var f) (Const (Integer 1))))
  =========== CPS ================
  Letcont k {} x2 =
      halt x2
  in
  Letval c = i32 2 in
  Letcont j {} y =
      Letrec
        f = (Fun k1 {} [x] ->
          Letprim r = Add2 (x, y) in
          Letcont k2 {} x3 =
              Continue k1 {} x3
          in
          g k2 {} [r])
        g = (Fun k3 {} [x1] ->
          Letval c1 = i32 4 in
          Letprim r1 = GT (c1, x1) in
          Letcont branch {} x4 =
              Letprim r2 = Add2 (x1, y) in
              Letcont k4 {} x5 =
                  Continue k3 {} x5
              in
              f k4 {} [r2]
          in
          Letcont branch1 {} x6 =
              Continue k3 {} x1
          in
          switch r1:
            1 -> Letval c2 = () in
                 Continue branch {} c2
            0 -> Letval c3 = () in
                 Continue branch1 {} c3)
      in
      Letval c4 = i32 1 in
      Letcont k5 {} x7 =
          Continue k {} x7
      in
      f k5 {} [c4]
  in
  Continue j {} c
  =========== Closure Passing Style ================
  Letcont code_k {env} x2 =
      halt x2
  in
  Letval k = (code_k) in
  Letval c = i32 2 in
  Letcont code_j {env1} y =
      Letsel k6 = select 1 env1 in
      Letrec
        f1 = (Fun k1 {env3} [x] ->
          Letsel env4 = select 1 env3 in
          Letval f = (f1, env4) in
          Letval g = (g1, env4) in
          Letsel y1 = select 0 env4 in
          Letprim r = Add2 (x, y1) in
          Letcont code_k2 {env5} x3 =
              Letsel k9 = select 1 env5 in
              Letsel k10 = select 0 k9 in
              Continue k10 {k9} x3
          in
          Letval k2 = (code_k2, k1) in
          Letsel $1 = select 0 g in
          $1 k2 {g} [r])
        g1 = (Fun k3 {env6} [x1] ->
          Letsel env7 = select 1 env6 in
          Letval f = (f1, env7) in
          Letval g = (g1, env7) in
          Letsel y1 = select 0 env7 in
          Letval c1 = i32 4 in
          Letprim r1 = GT (c1, x1) in
          Letcont code_branch {env8} x4 =
              Letsel f2 = select 4 env8 in
              Letsel k11 = select 3 env8 in
              Letsel y2 = select 2 env8 in
              Letsel x8 = select 1 env8 in
              Letprim r2 = Add2 (x8, y2) in
              Letcont code_k3 {env9} x5 =
                  Letsel k12 = select 1 env9 in
                  Letsel k13 = select 0 k12 in
                  Continue k13 {k12} x5
              in
              Letval k4 = (code_k3, k11) in
              Letsel $2 = select 0 f2 in
              $2 k4 {f2} [r2]
          in
          Letval
            branch = (code_branch, x1, y1, k3, f)
          in
          Letcont code_branch1 {env10} x6 =
              Letsel x9 = select 2 env10 in
              Letsel k14 = select 1 env10 in
              Letsel k15 = select 0 k14 in
              Continue k15 {k14} x9
          in
          Letval branch1 = (code_branch1, k3, x1) in
          switch r1:
            1 -> Letval c2 = () in
                 Letsel k16 = select 0 branch in
                 Continue k16 {branch} c2
            0 -> Letval c3 = () in
                 Letsel k17 = select 0 branch1 in
                 Continue k17 {branch1} c3)
      in
      Letval env_ = (y) in
      Letval f = (f1, env_) in
      Letval g = (g1, env_) in
      Letval c4 = i32 1 in
      Letcont code_k1 {env2} x7 =
          Letsel k7 = select 1 env2 in
          Letsel k8 = select 0 k7 in
          Continue k8 {k7} x7
      in
      Letval k5 = (code_k1, k6) in
      Letsel $0 = select 0 f in
      $0 k5 {f} [c4]
  in
  Letval j = (code_j, k) in
  Letsel k18 = select 0 j in
  Continue k18 {j} c
  =========== Flat ================
  main():
      k = (code_k)
      c = 2
      j = (code_j, k)
      k18 = j[0]
      k18(j, c)
  
  code_k1(env2, x7):
      k7 = env2[1]
      k8 = k7[0]
      k8(k7, x7)
  
  code_k2(env5, x3):
      k9 = env5[1]
      k10 = k9[0]
      k10(k9, x3)
  
  f1(env3, k1, x):
      env4 = env3[1]
      f = (f1, env4)
      g = (g1, env4)
      y1 = env4[0]
      r = PrimOp Add2 [x,y1]
      k2 = (code_k2, k1)
      $1 = g[0]
      $1(g, k2, r)
  
  code_branch(env8, x4):
      f2 = env8[4]
      k11 = env8[3]
      y2 = env8[2]
      x8 = env8[1]
      r2 = PrimOp Add2 [x8,y2]
      k4 = (code_k3, k11)
      $2 = f2[0]
      $2(f2, k4, r2)
  
  code_branch1(env10, x6):
      x9 = env10[2]
      k14 = env10[1]
      k15 = k14[0]
      k15(k14, x9)
  
  code_k3(env9, x5):
      k12 = env9[1]
      k13 = k12[0]
      k13(k12, x5)
  
  g1(env6, k3, x1):
      env7 = env6[1]
      f = (f1, env7)
      g = (g1, env7)
      y1 = env7[0]
      c1 = 4
      r1 = PrimOp GT [c1,x1]
      branch = (code_branch, x1, y1, k3, f)
      branch1 = (code_branch1, k3, x1)
      c2 = Unit
      k16 = branch[0]
      c3 = Unit
      k17 = branch1[0]
      switch r1:
          1 -> k16(branch, c2)
          0 -> k17(branch1, c3)
  
  code_j(env1, y):
      k6 = env1[1]
      env_ = (y)
      f = (f1, env_)
      g = (g1, env_)
      c4 = 1
      k5 = (code_k1, k6)
      $0 = f[0]
      $0(f, k5, c4)
  
  code_k(env, x2):
      exit x2
  
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
    const c = 2
    const j = [_notnull(code_j),_notnull(k)]
    const k18 = _notnull(j[0])
    k18(j,c)
  }
  function code_k1(env2,x7){
    const k7 = _notnull(env2[1])
    const k8 = _notnull(k7[0])
    k8(k7,x7)
  }
  function code_k2(env5,x3){
    const k9 = _notnull(env5[1])
    const k10 = _notnull(k9[0])
    k10(k9,x3)
  }
  function f1(env3,k1,x){
    const env4 = _notnull(env3[1])
    const f = [_notnull(f1),_notnull(env4)]
    const g = [_notnull(g1),_notnull(env4)]
    const y1 = _notnull(env4[0])
    const r = x + y1
    const k2 = [_notnull(code_k2),_notnull(k1)]
    const $1 = _notnull(g[0])
    $1(g,k2,r)
  }
  function code_branch(env8,x4){
    const f2 = _notnull(env8[4])
    const k11 = _notnull(env8[3])
    const y2 = _notnull(env8[2])
    const x8 = _notnull(env8[1])
    const r2 = x8 + y2
    const k4 = [_notnull(code_k3),_notnull(k11)]
    const $2 = _notnull(f2[0])
    $2(f2,k4,r2)
  }
  function code_branch1(env10,x6){
    const x9 = _notnull(env10[2])
    const k14 = _notnull(env10[1])
    const k15 = _notnull(k14[0])
    k15(k14,x9)
  }
  function code_k3(env9,x5){
    const k12 = _notnull(env9[1])
    const k13 = _notnull(k12[0])
    k13(k12,x5)
  }
  function g1(env6,k3,x1){
    const env7 = _notnull(env6[1])
    const f = [_notnull(f1),_notnull(env7)]
    const g = [_notnull(g1),_notnull(env7)]
    const y1 = _notnull(env7[0])
    const c1 = 4
    const r1 = c1 > x1
    const branch = [_notnull(code_branch),_notnull(x1),_notnull(y1),_notnull(k3),_notnull(f)]
    const branch1 = [_notnull(code_branch1),_notnull(k3),_notnull(x1)]
    const c2 = 0
    const k16 = _notnull(branch[0])
    const c3 = 0
    const k17 = _notnull(branch1[0])
    switch (Number(r1)){
      case 1: k16(branch,c2); break
      case 0: k17(branch1,c3); break
    }
  }
  function code_j(env1,y){
    const k6 = _notnull(env1[1])
    const env_ = [_notnull(y)]
    const f = [_notnull(f1),_notnull(env_)]
    const g = [_notnull(g1),_notnull(env_)]
    const c4 = 1
    const k5 = [_notnull(code_k1),_notnull(k6)]
    const $0 = _notnull(f[0])
    $0(f,k5,c4)
  }
  function code_k(env,x2){
  
    console.log(x2)
  }
