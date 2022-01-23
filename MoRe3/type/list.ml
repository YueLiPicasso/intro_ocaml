open More
open More.Core
open Common

module T = struct
  type ('a, 'b) t = Nil | Cons of 'a * 'b
  type ('a, 'b) logic  = ('a, 'b) t Core.logic
  type ('a, 'b) ilogic = ('a, 'b) t Core.ilogic
  let fmap  = fun f g -> function
    | Nil -> Nil
    | Cons (h , t) -> Cons (f h ,  g t)
end

module FT =  Fmap2(T)
    
include T

let nil = fun () -> inj Nil

let cons = fun x y -> inj @@ Cons(x,y)

let reify = FT.reify

module Rec = struct
  
  type 'a logic = ('a, 'b) t Core.logic as 'b
      
  type 'a ilogic = ('a, 'b) t Core.ilogic as 'b
   
  let rec reify = fun ra () -> FT.reify ra (reify ra) ()
      
end

module Seq = struct
  
  type 'a logic = unit -> ('a, 'b) t Core.logic as 'b

  type 'a ilogic = unit -> ('a, 'b) t Core.ilogic as 'b

  let rec ints n = fun () -> cons (inj n) (ints (n + 1))
                        
  let rec reify = fun ra () -> FT.Lazy.reify ra (reify ra) ()  
end

let rec take n l =
  let l = l () in if n <= 0  then Value Nil else
    match l with
    | Value Nil as v -> v
    | Value (Cons (h,t)) -> Value (Cons (h, take (n-1) t))
    | Var _ as v -> v
                              
