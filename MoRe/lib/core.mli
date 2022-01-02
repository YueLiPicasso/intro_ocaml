type 'a logic = Value of 'a | Var of Var.t

type 'a ilogic

val inj : 'a -> 'a ilogic

module Env : sig
  type 'a t
  val return : 'a -> 'a t
  val fmap   : ('a -> 'b) -> 'a t -> 'b t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t
  module Lazy : sig
    val bind   : 'a t Lazy.t -> ('a -> 'b t) -> 'b t
  end
end

module State : sig
  type 'a t
  val extract : 'a t -> 'a
  val extend  : 'a t -> ('a t -> 'b) -> 'b t
  val observe : 'a ilogic t -> 'a logic      
end

                 
exception Not_a_value

module Reifier : sig
  type ('a, 'b) t = ('a -> 'b) Env.t
  val reify   : ('a ilogic, 'a logic) t
  val prj     : (Var.t -> 'a) -> ('a ilogic, 'a) t
  val prj_exn : ('a ilogic, 'a) t
  val apply   : ('a, 'b) t -> 'a State.t -> 'b
  val compose : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val fmap    : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  val fcomap  : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  module Lazy : sig
    val apply   : ('a, 'b) t Lazy.t -> 'a State.t -> 'b
    val bind : ('a, 'b) t Lazy.t -> (('a, 'b) t Lazy.t State.t -> 'c Env.t) -> 'c Env.t
    val force : ('a, 'b) t Lazy.t State.t -> 'a -> 'b
  end
end

val fresh : ('a ilogic -> 'b Env.t) -> 'b Env.t                   

val run : ('a ilogic -> 'b ilogic Env.t) -> 'b ilogic State.t
  
               
