open Eucpp
open Eucpp.Core
    
type 'a t = 'a option
    
type 'a logic = 'a t Core.logic
    
type 'a ilogic = 'a t Core.ilogic
    
val fmap : ('a -> 'b) -> 'a t -> 'b t
    
val reify : ('a, 'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t
    
