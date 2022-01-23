type 'a logic = Value of 'a | Var of Var.t

type 'a ilogic

val inj : 'a -> 'a ilogic

module Env : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : (unit -> 'a t) -> ((unit -> 'a) -> 'b t) -> 'b t   
end


module State : sig
  type 'a t
end

module Reifier : sig
  type ('a, 'b) t = unit -> ('a -> 'b) Env.t 
  val reify  : ('a ilogic, 'a logic) t
  val apply  : ('a, 'b) t -> 'a State.t -> 'b
end

val fresh : ('a ilogic -> 'b Env.t) -> 'b Env.t                   

val run : ('a ilogic -> 'b Env.t) -> 'b State.t
  
val (>>=) : (unit -> 'a Env.t)  -> ((unit -> 'a) -> 'b Env.t) -> 'b Env.t

