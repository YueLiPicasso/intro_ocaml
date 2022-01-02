open More
open More.Core

type ('a, 'b) t = Nil | Cons of 'a * 'b

type 'a logic = ('a, 'a logic) t Core.logic

type 'a ilogic = ('a, 'a ilogic) t Core.ilogic

let nil = fun () -> inj Nil

let cons = fun x y -> inj @@ Cons(x,y)
  
let fmap  =
  fun f g -> function
    | Nil -> Nil
    | Cons (h , t) -> Cons (f h ,  g t)

let rec reify =
  fun ra -> lazy
    (Reifier.reify >>= (fun r -> ra >>= (fun fa -> reify ra >>>= (fun fr ->
         Env.return (fun x -> match r x with
             | Var _ as v -> v
             | Value t -> Value (fmap fa (Reifier.Lazy.force fr) t)))))) 
  
    
