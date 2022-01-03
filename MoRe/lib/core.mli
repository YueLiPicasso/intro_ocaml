type 'a logic = Value of 'a | Var of Var.t

type 'a ilogic

val inj : 'a -> 'a ilogic

module Env : sig
  type 'a t
  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t
  module Lazy : sig
    val bind : 'a t Lazy.t -> ('a Lazy.t -> 'b t) -> 'b t
  end
end

module State : sig
  type 'a t
end

module Reifier : sig
  type ('a, 'b) t = ('a -> 'b) Env.t
  val reify   : ('a ilogic, 'a logic) t
  val apply   : ('a, 'b) t -> 'a State.t -> 'b
  module Lazy : sig 
    val apply   : ('a, 'b) t Lazy.t -> 'a State.t -> 'b
  end
end

val fresh : ('a ilogic -> 'b Env.t) -> 'b Env.t                   

val run : ('a ilogic -> 'b ilogic Env.t) -> 'b ilogic State.t
  
val (>>=) : 'a Env.t -> ('a -> 'b Env.t) -> 'b Env.t

val (>>>=) : 'a Env.t Lazy.t -> ('a Lazy.t -> 'b Env.t) -> 'b Env.t

