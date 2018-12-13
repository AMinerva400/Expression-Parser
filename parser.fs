open System;
open Microsoft.FSharp.Math;;
open System.Collections.Generic;;
open CSC7B;;

newoperator "%" 600;; //May need to adjust precedence
binops <- List.append binops ["%"];;

let originalEval = eval;

let rec toStr x = function
  | Var(v) ->
    if v = x then Var(v+v)
    else Var(v)
  | Binop(op,a,b) ->
    Binop(op,(toStr x a),(toStr x b))
  | Uniop(op,a) ->
    Uniop(op,(toStr x a))
  | Ternop(op,v,a,b) ->
    Ternop(op,(toStr x v),(toStr x a),(toStr x b))
  | x -> x;;

eval <- fun (env:Dictionary<string,int>) exp ->
  match exp with
    | Val(v) -> 
      Console.Write("push ") 
      Console.WriteLine(v)
      v
    | Binop("+",a,b) -> 
      let ea = (eval env a)
      let eb = (eval env b)
      printfn "pop ax"
      printfn "pop bx"
      printfn "add bx ax"
      printfn "push ax"
      ea + eb
    | Binop("-",a,b) ->
      let ea = (eval env a)
      let eb = (eval env b)
      printfn "pop ax"
      printfn "pop bx"
      printfn "sub ax bx"
      printfn "push bx"
      ea - eb
    | Binop("*",a,b) ->
      let ea = (eval env a)
      let eb = (eval env b)
      printfn "pop ax"
      printfn "pop bx"
      printfn "imul bx ax"
      printfn "push ax"
      ea * eb
    | Binop("/",a,b) ->
      let ea = (eval env a)
      let eb = (eval env b)
      printfn "pop ax"
      printfn "pop bx"
      printfn "idiv ax bx"
      printfn "push bx"
      ea / eb
    | Binop("%",a,b) ->
      let ea = (eval env a)
      let eb = (eval env b)
      printfn "pop ax"
      printfn "pop bx"
      printfn "idiv ax bx"
      printfn "push dx"
      ea % eb
    | Uniop("-",a) ->
      let ea = (eval env a)
      printfn "pop ax"
      printfn "mov -1 bx"
      printfn "imul bx ax"
      printfn "push ax"
      -1 * ea
    | Ternop("let",Var(x),a,b) ->
      if (env.ContainsKey x) then 
        env.[x+x] <- (eval env a)
        eval env (toStr x b)
      else 
        env.[x] <- (eval env a)
        eval env b
    | e -> originalEval env e;;

advice_io(true,run);;