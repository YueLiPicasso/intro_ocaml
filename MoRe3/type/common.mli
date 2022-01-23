open More
open More.Core

module type T1 = sig
  type 'a t
  type 'a logic = 'a t Core.logic
  type 'a ilogic = 'a t Core.ilogic
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

module type T2 = sig
  type ('a, 'b) t
  type ('a, 'b) logic  = ('a, 'b) t Core.logic
  type ('a, 'b) ilogic = ('a, 'b) t Core.ilogic
  val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
end

module Fmap1 (T : T1) : sig
  val reify : ('a, 'b) Reifier.t -> ('a T.ilogic, 'b T.logic) Reifier.t
  module Lazy : sig
    val reify :
      ('a, 'b) Reifier.t ->
      (unit -> 'a T.ilogic, unit -> 'b T.logic) Reifier.t
  end
end

module Fmap2 (T : T2) : sig
  val reify :
    ('a1, 'b1) Reifier.t ->
    ('a2, 'b2) Reifier.t ->
    (('a1, 'a2) T.ilogic, ('b1, 'b2) T.logic) Reifier.t
      
  module Lazy : sig
    val reify :
      ('a1, 'b1) Reifier.t ->
      ('a2, 'b2) Reifier.t ->
      (unit -> ('a1, 'a2) T.ilogic, unit -> ('b1, 'b2) T.logic) Reifier.t
  end
end
