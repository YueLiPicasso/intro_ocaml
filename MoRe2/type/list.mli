open More
open More.Core

type ('a, 'b) t = Nil | Cons of 'a * 'b

type 'a logic = ('a, 'a logic) t Core.logic

type 'a ilogic = ('a, 'a ilogic) t Core.ilogic

val nil : unit -> 'a ilogic

val cons : 'a -> 'a ilogic  -> 'a ilogic 

val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t

val reify : ('a, 'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t 

module Seq : sig
  
  type 'a logic = unit -> 'a logic_node
  and 'a logic_node = ('a, 'a logic) t Core.logic

  type 'a ilogic = unit -> 'a ilogic_node
  and 'a ilogic_node = ('a, 'a ilogic) t Core.ilogic

  val ints : int -> int Core.ilogic ilogic
  
  val reify : ('a, 'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t
      
end

val take : n:int -> 'a Seq.logic -> 'a logic
