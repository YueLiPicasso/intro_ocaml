open More
open More.Core
    
type 'a t = 'a option
    
type 'a logic = 'a t Core.logic
    
type 'a ilogic = 'a t Core.ilogic

val none : unit-> 'a ilogic

val some : 'a -> 'a ilogic

val fmap : ('a -> 'b) -> 'a t -> 'b t
    
val reify : ('a, 'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t

module Nested : sig

  type nonrec logic  = 'b logic as 'b
    
  type nonrec ilogic = 'b ilogic as 'b
  
  val some : unit -> ilogic
    
  val reify : (ilogic,logic) Reifier.t Lazy.t

end


