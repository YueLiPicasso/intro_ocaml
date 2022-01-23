open More
open More.Core
open Common

module T = struct
  
  type ('a, 'b) t = Left of 'a | Right of 'b

  type ('a, 'b) logic = ('a, 'b) t Core.logic

  type ('a, 'b) ilogic = ('a, 'b) t Core.ilogic

  let fmap = fun left right -> function
    | Left v  -> Left (left v)
    | Right v -> Right (right v)

end

include T

module FT = Fmap2(T)
    
let left = fun v -> inj (Left v)

let right = fun v -> inj (Right v)

let reify = FT.reify 
