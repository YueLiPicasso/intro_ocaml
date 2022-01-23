open More
open More.Core

type ('a, 'b) t = Nil | Cons of 'a * 'b

val nil : unit -> ('a, 'b) t Core.ilogic

val cons : 'a -> 'b -> ('a, 'b) t Core.ilogic

val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t

val reify :
  ('a1, 'b1) Reifier.t ->
  ('a2, 'b2) Reifier.t ->
  (('a1, 'a2) t Core.ilogic, ('b1, 'b2) t Core.logic) Reifier.t
     
(* eager list *)
module Rec : sig
  
  type 'a logic = ('a, 'b) t Core.logic as 'b
      
  type 'a ilogic = ('a, 'b) t Core.ilogic as 'b

  val reify : ('a, 'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t
      
end

(* lazy list *)
module Seq : sig
  
  type 'a logic = unit -> ('a, 'a logic) t Core.logic

  type 'a ilogic = unit -> ('a, 'a ilogic) t Core.ilogic

  val ints : int -> int Core.ilogic ilogic
  
  val reify : ('a, 'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t
      
end

val take : int -> 'a Seq.logic -> 'a Rec.logic
