type env

type index
   
type t

val anchor : t -> Anchor.t

val env    : t -> env

val index  : t -> index

val tag    : int

val size   : int

val fresh  : env -> t

val fresh_env : unit ->  env

exception Scope_violation of t 
