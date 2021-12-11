type 'a t

val var : Var.env -> 'a t -> Var.t option
  
val fresh : Var.env -> 'a t
