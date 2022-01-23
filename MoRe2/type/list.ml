open More
open More.Core

type ('a, 'b) t = Nil | Cons of 'a * 'b

let nil = fun () -> inj Nil

let cons = fun x y -> inj @@ Cons(x,y)

let fmap  =
  fun f g -> function
    | Nil -> Nil
    | Cons (h , t) -> Cons (f h ,  g t)

module Abs = struct
  let reify = fun ra rb -> EL.Eager
      (Reifier.reify >>= (fun r -> ra >>= (fun fa -> rb >>= (fun fb ->
           Env.return (fun x -> match EL.use r x with
               | Var _ as v -> v
               | Value t -> Value (fmap (EL.use fa) (EL.use fb) t))))))
end

module Rec = struct
  
  type 'a logic = ('a, 'a logic) t Core.logic
      
  type 'a ilogic = ('a, 'a ilogic) t Core.ilogic
   
  let rec reify = fun ra -> EL.Lazy
      (lazy (Reifier.reify >>= (fun r -> ra >>= (fun fa -> reify ra >>= (fun fr ->
           Env.return (fun x -> match EL.use r x with
               | Var _ as v -> v
               | Value t -> Value (fmap (EL.use fa) (EL.use fr) t)))))))   
      
end

module Seq = struct
  
  type 'a logic = unit -> 'a logic_node
  and 'a logic_node = ('a, 'a logic) t Core.logic

  type 'a ilogic = unit -> 'a ilogic_node
  and 'a ilogic_node = ('a, 'a ilogic) t Core.ilogic

  let rec ints n = fun () -> cons (inj n) (ints (n + 1))
                        
  let rec reify = fun ra -> EL.Lazy
    (lazy (Reifier.reify >>= (fun r -> ra >>= (fun fa -> reify ra >>= (fun fr ->
         Env.return (fun x -> match EL.use r @@ x () with
             | Var _ as v -> fun () -> v
             | Value t -> fun () -> Value (fmap (EL.use fa) (EL.use fr) t)))))))    
end

let rec take ~n l = let l = l () in if n <= 0 || l = Value Nil then Value Nil
  else let Value (Cons (h,t)) = l in Value (Cons (h, take (n-1) t))
