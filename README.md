# Effektos

This repo is an experimental project aiming to build a functional-style language with CPS IR.

Here are some features currently in progress:

- Effect Handlers

  A powerful way to abstract control-flow, exception and even coroutine in user space. 

  Here is an example to implement and use state effects, without any mutable variable:

  ```
    let print = fun x -> (extern "console.log" x) in 
    effect Put, Get, Return in 
    let runState = 
    fun initial -> fun program -> 
        let s = 
        handle program () with 
        | Put (x,k) -> fun ignored -> (k ()) x 
        | Get (ignored,k) -> fun y -> (k y) y 
        | Return (x,k) -> fun ignored -> x 
        in s initial 
    in 
    let result =
        runState 5 (fun ignored -> 
            let x = Get () in 
            print x; 
            Put (x + 10); 
            let x2 = Get () in 
            Return x2;  
            print 114514 
        )
    in 
    print result
  ```   
  
  output:
  ```
  5
  15
  ```

- Pattern Matching 

  Many "modern" langauges have proposed or recently implemented this feature, which is already widely used in most functional langauges.  


- General Optimization

  Inlining, constant folding and more.

- Javascript Backend

- C Backend
   

And some task plan to do in future:

- Type Checking & Inference

- Imperative features base on it's functional core

- References Counting based GC

- Native Code Backend






