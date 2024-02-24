  $ effektos --show-input --debug-tokens --debug-lambda --debug-uniquify --debug-cps --debug-simplify --debug-closure-conversion --debug-flat --debug-js $TESTDIR/closure_table.tos
  =========== Source ================
  let empty = fun key -> () in 
  let append = fun k -> fun v -> fun m -> 
             (fun x -> if x == k then v else m x) in 
  let table = append 1 2 empty in 
  table 4
  
  =========== Tokens ================
  [ (Symbol "let",1:1,3)
  , (Identifier "empty",1:5,5)
  , (Symbol "=",1:11,1)
  , (Symbol "fun",1:13,3)
  , (Identifier "key",1:17,3)
  , (Symbol "->",1:21,2)
  , (Symbol "(",1:24,1)
  , (Symbol ")",1:25,1)
  , (Symbol "in",1:27,2)
  , (Symbol "let",2:1,3)
  , (Identifier "append",2:5,6)
  , (Symbol "=",2:12,1)
  , (Symbol "fun",2:14,3)
  , (Identifier "k",2:18,1)
  , (Symbol "->",2:20,2)
  , (Symbol "fun",2:23,3)
  , (Identifier "v",2:27,1)
  , (Symbol "->",2:29,2)
  , (Symbol "fun",2:32,3)
  , (Identifier "m",2:36,1)
  , (Symbol "->",2:38,2)
  , (Symbol "(",3:12,1)
  , (Symbol "fun",3:13,3)
  , (Identifier "x",3:17,1)
  , (Symbol "->",3:19,2)
  , (Symbol "if",3:22,2)
  , (Identifier "x",3:25,1)
  , (Symbol "==",3:27,2)
  , (Identifier "k",3:30,1)
  , (Symbol "then",3:32,4)
  , (Identifier "v",3:37,1)
  , (Symbol "else",3:39,4)
  , (Identifier "m",3:44,1)
  , (Identifier "x",3:46,1)
  , (Symbol ")",3:47,1)
  , (Symbol "in",3:49,2)
  , (Symbol "let",4:1,3)
  , (Identifier "table",4:5,5)
  , (Symbol "=",4:11,1)
  , (Identifier "append",4:13,6)
  , (Number 1,4:20,1)
  , (Number 2,4:22,1)
  , (Identifier "empty",4:24,5)
  , (Symbol "in",4:30,2)
  , (Identifier "table",5:1,5)
  , (Number 4,5:7,1)
  , (Eof,0:0,0)
  ]
  =========== Lambda ================
  Let empty
      ( Abs key ( Const Unit ) )
      ( Let append
          ( Abs k
              ( Abs v
                  ( Abs m
                      ( Abs x
                          ( Switch
                              ( PrimOp EQ
                                  [ Var x
                                  , Var k
                                  ]
                              )
                              [
                                  ( 1
                                  , Var v
                                  )
                              ,
                                  ( 0
                                  , App ( Var m ) ( Var x )
                                  )
                              ] Nothing
                          )
                      )
                  )
              )
          )
          ( Let table
              ( App
                  ( App
                      ( App ( Var append )
                          ( Const
                              ( Integer 1 )
                          )
                      )
                      ( Const
                          ( Integer 2 )
                      )
                  ) ( Var empty )
              )
              ( App ( Var table )
                  ( Const
                      ( Integer 4 )
                  )
              )
          )
      )
  =========== Uniquified ================
  Let empty (Abs key (Const Unit)) (Let append (Abs k (Abs v (Abs m (Abs x (Switch (PrimOp EQ [Var x,Var k]) [(1,Var v),(0,App (Var m) (Var x))] Nothing))))) (Let table (App (App (App (Var append) (Const (Integer 1))) (Const (Integer 2))) (Var empty)) (App (Var table) (Const (Integer 4)))))
  =========== CPS ================
  Letcont k1 {} x1 =
      halt x1
  in
  Letval
    f =
      (Fun k2 {} [key] ->
        Letval c = () in
        Continue k2 {} c)
  in
  Letcont j {} empty =
      Letval
        f1 =
          (Fun k3 {} [k] ->
            Letval
              f2 =
                (Fun k4 {} [v] ->
                  Letval
                    f3 =
                      (Fun k5 {} [m] ->
                        Letval
                          f4 =
                            (Fun k6 {} [x] ->
                              Letprim
                                r = EQ (x, k)
                              in
                              Letcont branch {} x2 =
                                  Continue k6 {} v
                              in
                              Letcont
                                branch1 {} x3 =
                                  Letcont k7 {} x4 =
                                      Continue k6 {} x4
                                  in
                                  m k7 {} [x]
                              in
                              switch r:
                                1 -> Letval
                                       c1 = ()
                                     in
                                     Continue branch {} c1
                                0 -> Letval
                                       c2 = ()
                                     in
                                     Continue branch1 {} c2)
                        in
                        Continue k5 {} f4)
                  in
                  Continue k4 {} f3)
            in
            Continue k3 {} f2)
      in
      Letcont j1 {} append =
          Letval c3 = i32 1 in
          Letcont k10 {} x7 =
              Letval c4 = i32 2 in
              Letcont k9 {} x6 =
                  Letcont k8 {} x5 =
                      Letcont j2 {} table =
                          Letval c5 = i32 4 in
                          Letcont k11 {} x8 =
                              Continue k1 {} x8
                          in
                          table k11 {} [c5]
                      in
                      Continue j2 {} x5
                  in
                  x6 k8 {} [empty]
              in
              x7 k9 {} [c4]
          in
          append k10 {} [c3]
      in
      Continue j1 {} f1
  in
  Continue j {} f
  =========== Closure Passing Style ================
  Letcont code_k {env} x1 =
      halt x1
  in
  Letval k1 = (code_k) in
  Letval
    code_f =
      (Fun k2 {env1} [key] ->
        Letval c = () in
        Letsel k12 = select 0 k2 in
        Continue k12 {k2} c)
  in
  Letval f = (code_f) in
  Letcont code_j {env2} empty =
      Letsel k13 = select 1 env2 in
      Letval
        code_f1 =
          (Fun k3 {env3} [k] ->
            Letval
              code_f2 =
                (Fun k4 {env4} [v] ->
                  Letsel k14 = select 1 env4 in
                  Letval
                    code_f3 =
                      (Fun k5 {env5} [m] ->
                        Letsel v1 = select 2 env5 in
                        Letsel k15 =
                          select 1 env5
                        in
                        Letval
                          code_f4 =
                            (Fun k6 {env6} [x] ->
                              Letsel m1 =
                                select 3 env6
                              in
                              Letsel v2 =
                                select 2 env6
                              in
                              Letsel k16 =
                                select 1 env6
                              in
                              Letprim
                                r = EQ (x, k16)
                              in
                              Letcont
                                code_branch {env7} x2 =
                                  Letsel v3 =
                                    select 2 env7
                                  in
                                  Letsel k17 =
                                    select 1 env7
                                  in
                                  Letsel k18 =
                                    select 0 k17
                                  in
                                  Continue k18 {k17} v3
                              in
                              Letval
                                branch =
                                  (code_branch, k6, v2)
                              in
                              Letcont
                                code_branch1 {env8} x3 =
                                  Letsel x9 =
                                    select 3 env8
                                  in
                                  Letsel m2 =
                                    select 2 env8
                                  in
                                  Letsel k19 =
                                    select 1 env8
                                  in
                                  Letcont
                                    code_k1 {env9} x4 =
                                      Letsel k20 =
                                        select 1 env9
                                      in
                                      Letsel k21 =
                                        select 0 k20
                                      in
                                      Continue k21 {k20} x4
                                  in
                                  Letval
                                    k7 =
                                      (code_k1, k19)
                                  in
                                  Letsel $0 =
                                    select 0 m2
                                  in
                                  $0 k7 {m2} [x9]
                              in
                              Letval
                                branch1 =
                                  (code_branch1, k6, m1, x)
                              in
                              switch r:
                                1 -> Letval
                                       c1 = ()
                                     in
                                     Letsel k22 =
                                       select 0 branch
                                     in
                                     Continue k22 {branch} c1
                                0 -> Letval
                                       c2 = ()
                                     in
                                     Letsel k23 =
                                       select 0 branch1
                                     in
                                     Continue k23 {branch1} c2)
                        in
                        Letval
                          f4 = (code_f4, k15, v1, m)
                        in
                        Letsel k24 = select 0 k5 in
                        Continue k24 {k5} f4)
                  in
                  Letval f3 = (code_f3, k14, v) in
                  Letsel k25 = select 0 k4 in
                  Continue k25 {k4} f3)
            in
            Letval f2 = (code_f2, k) in
            Letsel k26 = select 0 k3 in
            Continue k26 {k3} f2)
      in
      Letval f1 = (code_f1) in
      Letcont code_j1 {env10} append =
          Letsel empty1 = select 2 env10 in
          Letsel k27 = select 1 env10 in
          Letval c3 = i32 1 in
          Letcont code_k2 {env11} x7 =
              Letsel empty2 = select 2 env11 in
              Letsel k28 = select 1 env11 in
              Letval c4 = i32 2 in
              Letcont code_k3 {env12} x6 =
                  Letsel empty3 = select 2 env12 in
                  Letsel k29 = select 1 env12 in
                  Letcont code_k4 {env13} x5 =
                      Letsel k30 = select 1 env13 in
                      Letcont
                        code_j2 {env14} table =
                          Letsel k31 =
                            select 1 env14
                          in
                          Letval c5 = i32 4 in
                          Letcont
                            code_k5 {env15} x8 =
                              Letsel k32 =
                                select 1 env15
                              in
                              Letsel k33 =
                                select 0 k32
                              in
                              Continue k33 {k32} x8
                          in
                          Letval
                            k11 = (code_k5, k31)
                          in
                          Letsel $1 =
                            select 0 table
                          in
                          $1 k11 {table} [c5]
                      in
                      Letval j2 = (code_j2, k30) in
                      Letsel k34 = select 0 j2 in
                      Continue k34 {j2} x5
                  in
                  Letval k8 = (code_k4, k29) in
                  Letsel $2 = select 0 x6 in
                  $2 k8 {x6} [empty3]
              in
              Letval k9 = (code_k3, k28, empty2) in
              Letsel $3 = select 0 x7 in
              $3 k9 {x7} [c4]
          in
          Letval k10 = (code_k2, k27, empty1) in
          Letsel $4 = select 0 append in
          $4 k10 {append} [c3]
      in
      Letval j1 = (code_j1, k13, empty) in
      Letsel k35 = select 0 j1 in
      Continue k35 {j1} f1
  in
  Letval j = (code_j, k1) in
  Letsel k36 = select 0 j in
  Continue k36 {j} f
  =========== Flat ================
  main():
      k1 = (code_k)
      f = (code_f)
      j = (code_j, k1)
      k36 = j[0]
      k36(j, f)
  
  code_k1(env9, x4):
      k20 = env9[1]
      k21 = k20[0]
      k21(k20, x4)
  
  code_branch1(env8, x3):
      x9 = env8[3]
      m2 = env8[2]
      k19 = env8[1]
      k7 = (code_k1, k19)
      $0 = m2[0]
      $0(m2, k7, x9)
  
  code_branch(env7, x2):
      v3 = env7[2]
      k17 = env7[1]
      k18 = k17[0]
      k18(k17, v3)
  
  code_f4(env6, k6, x):
      m1 = env6[3]
      v2 = env6[2]
      k16 = env6[1]
      r = PrimOp EQ [x,k16]
      branch = (code_branch, k6, v2)
      branch1 = (code_branch1, k6, m1, x)
      c1 = Unit
      k22 = branch[0]
      c2 = Unit
      k23 = branch1[0]
      switch r:
          1 -> k22(branch, c1)
          0 -> k23(branch1, c2)
  
  code_f3(env5, k5, m):
      v1 = env5[2]
      k15 = env5[1]
      f4 = (code_f4, k15, v1, m)
      k24 = k5[0]
      k24(k5, f4)
  
  code_f2(env4, k4, v):
      k14 = env4[1]
      f3 = (code_f3, k14, v)
      k25 = k4[0]
      k25(k4, f3)
  
  code_k5(env15, x8):
      k32 = env15[1]
      k33 = k32[0]
      k33(k32, x8)
  
  code_j2(env14, table):
      k31 = env14[1]
      c5 = 4
      k11 = (code_k5, k31)
      $1 = table[0]
      $1(table, k11, c5)
  
  code_k4(env13, x5):
      k30 = env13[1]
      j2 = (code_j2, k30)
      k34 = j2[0]
      k34(j2, x5)
  
  code_k3(env12, x6):
      empty3 = env12[2]
      k29 = env12[1]
      k8 = (code_k4, k29)
      $2 = x6[0]
      $2(x6, k8, empty3)
  
  code_k2(env11, x7):
      empty2 = env11[2]
      k28 = env11[1]
      c4 = 2
      k9 = (code_k3, k28, empty2)
      $3 = x7[0]
      $3(x7, k9, c4)
  
  code_j1(env10, append):
      empty1 = env10[2]
      k27 = env10[1]
      c3 = 1
      k10 = (code_k2, k27, empty1)
      $4 = append[0]
      $4(append, k10, c3)
  
  code_f1(env3, k3, k):
      f2 = (code_f2, k)
      k26 = k3[0]
      k26(k3, f2)
  
  code_j(env2, empty):
      k13 = env2[1]
      f1 = (code_f1)
      j1 = (code_j1, k13, empty)
      k35 = j1[0]
      k35(j1, f1)
  
  code_f(env1, k2, key):
      c = Unit
      k12 = k2[0]
      k12(k2, c)
  
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
    const k1 = [_notnull(code_k)]
    const f = [_notnull(code_f)]
    const j = [_notnull(code_j),_notnull(k1)]
    const k36 = _notnull(j[0])
    k36(j,f)
  }
  function code_k1(env9,x4){
    const k20 = _notnull(env9[1])
    const k21 = _notnull(k20[0])
    k21(k20,x4)
  }
  function code_branch1(env8,x3){
    const x9 = _notnull(env8[3])
    const m2 = _notnull(env8[2])
    const k19 = _notnull(env8[1])
    const k7 = [_notnull(code_k1),_notnull(k19)]
    const $0 = _notnull(m2[0])
    $0(m2,k7,x9)
  }
  function code_branch(env7,x2){
    const v3 = _notnull(env7[2])
    const k17 = _notnull(env7[1])
    const k18 = _notnull(k17[0])
    k18(k17,v3)
  }
  function code_f4(env6,k6,x){
    const m1 = _notnull(env6[3])
    const v2 = _notnull(env6[2])
    const k16 = _notnull(env6[1])
    const r = x === k16
    const branch = [_notnull(code_branch),_notnull(k6),_notnull(v2)]
    const branch1 = [_notnull(code_branch1),_notnull(k6),_notnull(m1),_notnull(x)]
    const c1 = 0
    const k22 = _notnull(branch[0])
    const c2 = 0
    const k23 = _notnull(branch1[0])
    switch (Number(r)){
      case 1: k22(branch,c1); break
      case 0: k23(branch1,c2); break
    }
  }
  function code_f3(env5,k5,m){
    const v1 = _notnull(env5[2])
    const k15 = _notnull(env5[1])
    const f4 = [_notnull(code_f4),_notnull(k15),_notnull(v1),_notnull(m)]
    const k24 = _notnull(k5[0])
    k24(k5,f4)
  }
  function code_f2(env4,k4,v){
    const k14 = _notnull(env4[1])
    const f3 = [_notnull(code_f3),_notnull(k14),_notnull(v)]
    const k25 = _notnull(k4[0])
    k25(k4,f3)
  }
  function code_k5(env15,x8){
    const k32 = _notnull(env15[1])
    const k33 = _notnull(k32[0])
    k33(k32,x8)
  }
  function code_j2(env14,table){
    const k31 = _notnull(env14[1])
    const c5 = 4
    const k11 = [_notnull(code_k5),_notnull(k31)]
    const $1 = _notnull(table[0])
    $1(table,k11,c5)
  }
  function code_k4(env13,x5){
    const k30 = _notnull(env13[1])
    const j2 = [_notnull(code_j2),_notnull(k30)]
    const k34 = _notnull(j2[0])
    k34(j2,x5)
  }
  function code_k3(env12,x6){
    const empty3 = _notnull(env12[2])
    const k29 = _notnull(env12[1])
    const k8 = [_notnull(code_k4),_notnull(k29)]
    const $2 = _notnull(x6[0])
    $2(x6,k8,empty3)
  }
  function code_k2(env11,x7){
    const empty2 = _notnull(env11[2])
    const k28 = _notnull(env11[1])
    const c4 = 2
    const k9 = [_notnull(code_k3),_notnull(k28),_notnull(empty2)]
    const $3 = _notnull(x7[0])
    $3(x7,k9,c4)
  }
  function code_j1(env10,append){
    const empty1 = _notnull(env10[2])
    const k27 = _notnull(env10[1])
    const c3 = 1
    const k10 = [_notnull(code_k2),_notnull(k27),_notnull(empty1)]
    const $4 = _notnull(append[0])
    $4(append,k10,c3)
  }
  function code_f1(env3,k3,k){
    const f2 = [_notnull(code_f2),_notnull(k)]
    const k26 = _notnull(k3[0])
    k26(k3,f2)
  }
  function code_j(env2,empty){
    const k13 = _notnull(env2[1])
    const f1 = [_notnull(code_f1)]
    const j1 = [_notnull(code_j1),_notnull(k13),_notnull(empty)]
    const k35 = _notnull(j1[0])
    k35(j1,f1)
  }
  function code_f(env1,k2,key){
    const c = 0
    const k12 = _notnull(k2[0])
    k12(k2,c)
  }
  function code_k(env,x1){
  
    console.log(x1)
  }

