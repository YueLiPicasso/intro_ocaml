open Eucpp
open Eucpp.Core

type 'a t = 'a option
    
type 'a logic = 'a t Core.logic
    
type 'a ilogic = 'a t Core.ilogic

let none = fun () -> inj None

let some = fun v -> inj (Some v)

let fmap = fun f -> function
  | Some a -> Some (f a)
  | None -> None

let reify = fun ra ->
  let (>>=) = Env.bind in
  (Reifier.reify >>= (fun r -> (ra >>= (fun fa ->
       Env.return (fun x ->
           match r x with
           | Var _ as v'   -> v'
           | Value t -> Value (fmap fa t))))))

module Nested = struct

  type nonrec logic =  'b logic as 'b
    
  type nonrec ilogic =  'b ilogic as 'b
    
  let rec some () = inj (Some (some()))

  let rec reify = lazy (Reifier.compose Reifier.reify 
      (Env.Lazy.bind reify (fun r ->
           Env.return (fun x ->
               match x with
               | Var _ as v' -> v'
               | Value t -> Value (fmap r t))))) 
end





      

