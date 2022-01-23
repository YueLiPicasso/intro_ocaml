open More
open More.Core
open Common

module T = struct
  type 'a t = 'a option
    
  type 'a logic = 'a t Core.logic
    
  type 'a ilogic = 'a t Core.ilogic

  let fmap = fun f -> function
    | Some a -> Some (f a)
    | None -> None

end

module FT =  Fmap1(T)
    
include T

let none = fun () -> inj None

let some = fun v -> inj (Some v)


let reify = FT.reify
    
module Rec = struct

  type logic  = 'b t Core.logic  as 'b
    
  type ilogic = 'b t Core.ilogic as 'b

  let rec reify = fun () ->  FT.reify reify ()
      
end

module Seq = struct

  type logic  = unit -> 'b t Core.logic  as 'b
    
  type ilogic = unit -> 'b t Core.ilogic as 'b
    
  let rec oo = fun () -> inj (Some (oo))

  let rec reify = fun () ->  FT.Lazy.reify reify ()
      
end

let rec take ~n oo = let oo = oo () in if n <= 0 then Value None else
    match oo with
    | Value None as v -> v
    | Value (Some x) -> Value (Some (take (n-1) x))





      

