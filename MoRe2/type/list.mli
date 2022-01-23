open More
open More.Core

type ('a, 'b) t = Nil | Cons of 'a * 'b

val nil : unit -> ('a, 'b) t Core.ilogic

val cons : 'a -> 'b -> ('a, 'b) t Core.ilogic

val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t

(* eager list *)
module Rec : sig
  
  type 'a logic = ('a, 'a logic) t Core.logic

  type 'a ilogic = ('a, 'a ilogic) t Core.ilogic

  val reify : ('a, 'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t
      
end


(* lazy list *)
module Seq : sig
  
  type 'a logic = unit -> 'a logic_node
  and 'a logic_node = ('a, 'a logic) t Core.logic

  type 'a ilogic = unit -> 'a ilogic_node
  and 'a ilogic_node = ('a, 'a ilogic) t Core.ilogic

  val ints : int -> int Core.ilogic ilogic
  
  val reify : ('a, 'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t
      
end

val take : n:int -> 'a Seq.logic -> 'a Rec.logic

module Abs : sig
  val reify : ('a1, 'b1) Reifier.t -> ('a2, 'b2) Reifier.t ->
    (('a1, 'a2) t Core.ilogic, ('b1, 'b2) t Core.logic) Reifier.t
end
