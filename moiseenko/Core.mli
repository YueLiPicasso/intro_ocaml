module Var : sig
  type t
     
  val idx : t -> int
end

(* Type `logic` --- either value or variable.
 * Exposing this type is okay.
 *)
type 'a logic = Value of 'a | Var of Var.t

(* Type `ilogic` --- (implicit) logic type.
 * (a.k.a `injected` in the current implementation)
 * At runtime `'a ilogic` has the same representation as `'a`.
 * It is unsafe to expose this type to an end user.
 *)
type 'a ilogic

val inj : 'a -> 'a ilogic

(* This exception is raised when the (implicit) logic variable is used outside of its scope
 * (i.g. in the different `run` statement).
 *)
exception Var_scope_violation of Var.t

module Env: sig
  (* `'a Env.t` --- essentially a reader monad *)
  type 'a t

  (* Usual boring monadic stuff *)

  val return : 'a -> 'a t
    
  val fmap : ('a -> 'b) -> 'a t -> 'b t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

end

module State : sig
  (* `'a State.t` --- essentially a coreader comonad, dual to '`a Env.t` *)
  type 'a t

  (* Usual comonadic stuff *)

  val extract : 'a t -> 'a

  val extend : 'a t -> ('a t -> 'b) -> 'b t

  (* Interesting part --- we can `observe` implicit logic variable, dipped into our comonad *)
  val observe : 'a ilogic t -> 'a logic

  end

exception Not_a_value

module Reifier : sig
  (* Reifier from type `'a` into type `'b` is an `'a -> 'b` function
   * dipped into the `Env.t` monad, will see how it plays later.
   * Perhaps, it is possible to not expose the reifier type and make it itself
   * a monad or something else that composes nicely, but I haven't figured out yet.
   *)
  type ('a, 'b) t = ('a -> 'b) Env.t

  (* Some predefined reifiers from which other reifiers will be composed *)

  (* this one transforms implicit logic value into regular logic value *)
  val reify : ('a ilogic, 'a logic) t

  (* this one projects implicit logic into the underlying type,
   * handling variables with the help of the user provided function
   *)
  val prj : (Var.t -> 'a) -> ('a ilogic, 'a) t

  (* this one projects implicit logic into the underlying type,
   * raising an exception if it finds a variable
   *)
  val prj_exn : ('a ilogic, 'a) t

  (* Interesting part --- we can apply a reifier to a value dipped into `State.t` comonad *)
  val apply : ('a, 'b) t -> 'a State.t -> 'b

  (* composition of two reifiers *)
  val compose : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

  (* Reifier is a profunctor, so we get combinators
   * to compose reifiers with regular functions
   *)

  val fmap : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

  val fcomap : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
end

(* Here the usual MiniKanrenish goal-related stuff should be given.
 * For simplicity everything except the `fresh` is omitted.
 *)

(* The 'real' type of the `fresh` is commented.
 * For testing purposes we use different type.
 *)

(* val fresh : ('a ilogic -> goal) -> goal *)
val fresh : ('a ilogic -> 'b Env.t) -> 'b Env.t

(* The 'real' type of the `run` is commented.
 * For testing purposes we use very simplified run.
 *)

(* val run : ('a ilogic -> goal) -> 'a ilogic State.t Stream.t *)
val run : ('a ilogic -> 'b ilogic Env.t) -> 'b ilogic State.t
