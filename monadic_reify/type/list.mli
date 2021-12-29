open Eucpp
open Eucpp.Core

type ('a, 'b) t = Nil | Cons of 'a * 'b

type 'a logic = ('a, 'a logic) t Core.logic

type 'a ilogic = ('a, 'a ilogic) t Core.ilogic

val nil : unit -> 'a ilogic

val cons : 'a -> 'a ilogic  -> 'a ilogic 

val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t

val reify : ('a, 'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t
