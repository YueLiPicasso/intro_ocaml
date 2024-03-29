open Eucpp
open Eucpp.Core
    
type 'a t = 'a option
    
type 'a logic = 'a t Core.logic
    
type 'a ilogic = 'a t Core.ilogic

val none : unit-> 'a ilogic

val some : 'a -> 'a ilogic

val somes : unit -> ('a ilogic as 'a)
  
val fmap : ('a -> 'b) -> 'a t -> 'b t
    
val reify : ('a, 'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t
    
val reify_inf : unit -> ('a ilogic as 'a, 'b logic as 'b) Reifier.t
