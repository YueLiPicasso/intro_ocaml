type 'a logic = Value of 'a | Var of Var.t

type 'a ilogic

val inj : 'a -> 'a ilogic


module EL : sig
   type 'a t =
      Eager of 'a
     | Lazy  of 'a Lazy.t

   val use : 'a t -> 'a
end

module Env : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t EL.t -> ('a EL.t -> 'b t) -> 'b t   
end


module State : sig
  type 'a t
end

module Reifier : sig
  type ('a, 'b) t = ('a -> 'b) Env.t EL.t
  val reify  : ('a ilogic, 'a logic) t
  val apply  : ('a, 'b) t -> 'a State.t -> 'b
end

val fresh : ('a ilogic -> 'b Env.t) -> 'b Env.t                   

val run : ('a ilogic -> 'b ilogic Env.t) -> 'b ilogic State.t
  
val (>>=) : 'a Env.t EL.t -> ('a EL.t -> 'b Env.t) -> 'b Env.t


