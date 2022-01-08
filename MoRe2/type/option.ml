open More
open More.Core

type 'a t = 'a option
    
type 'a logic = 'a t Core.logic
    
type 'a ilogic = 'a t Core.ilogic

let none = fun () -> inj None

let some = fun v -> inj (Some v)

let fmap = fun f -> function
  | Some a -> Some (f a)
  | None -> None

let reify = fun ra -> 
  EL.Eager (Reifier.reify >>= (fun r -> (ra >>= (fun fa ->
      Env.return (fun x ->
          match EL.use r x with
          | Var _ as v'   -> v'
          | Value t -> Value (fmap (EL.use fa) t))))))
    
module Nested = struct

  type nonrec logic =  'b logic as 'b
    
  type nonrec ilogic =  'b ilogic as 'b
    
  let rec some () = inj (Some (some()))

  let rec reify' = lazy
    (Reifier.reify >>= (fun r -> reify >>= (fun rr -> Env.return (fun x ->
         match EL.use r x with
         | Var _ as v -> v
         | Value t -> Value (fmap (EL.use rr) t)))))
  and reify = EL.Lazy reify'
end





      

