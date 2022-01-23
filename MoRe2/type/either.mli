open More
open More.Core

type ('a, 'b) t = Left of 'a | Right of 'b

type ('a, 'b) logic = ('a, 'b) t Core.logic

type ('a, 'b) ilogic = ('a, 'b) t Core.ilogic

val left : 'a -> ('a, 'b) ilogic

val right : 'b -> ('a, 'b) ilogic

val fmap : left:('a1 -> 'a2) -> right:('b1 -> 'b2) -> ('a1, 'b1) t -> ('a2, 'b2) t

val reify : left:('a1, 'a2) Reifier.t -> right:('b1, 'b2) Reifier.t ->
  (('a1, 'b1) ilogic, ('a2, 'b2) logic) Reifier.t
