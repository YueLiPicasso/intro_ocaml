open Logic
open Core

(** the type for ground rational numbers *)
@type 'a t = 'a * 'a with show, html, eq, compare, foldl, foldr, gmap, fmt

(** Type synonym to prevent toplevel [logic] from being hidden *)
@type 'a logic' = 'a logic with show, html, eq, compare, foldl, foldr, gmap, fmt

(** Ground rationals are ismorphic for regular one *)
@type ground = Nat.ground t with show, html, eq, compare, foldl, foldr, gmap, fmt

(** Logic rational *)
@type logic = Nat.logic t logic' with show, html, eq, compare, foldl, foldr, gmap, fmt

(** Logic injection (for reification) *)
val inj : ground -> logic

(** A type synonym for injected nat *)
type groundi = (ground, logic) injected

(** Reifier *)
val reify : VarEnv.t -> groundi -> logic

(* Shallow non-variable projection *)
val prjc : (int -> ground GT.list -> ground) -> VarEnv.t -> groundi -> ground

(** [of_int n] converts integer pair [(m,n)] into [ground]; negative integers become [O] *)
val of_int : int * int -> ground

(** [to_int g] converts ground [g] into integer pair *)
val to_int : ground -> int * int

(** Make injected [rational] from ground one *)
val rat : ground -> groundi

(** Relational addition *)
val addo  : groundi -> groundi -> groundi -> goal

(** Infix syninym for [addo] *)
val ( + ) : groundi -> groundi -> groundi -> goal

(** Relational multiplication *)
val mulo  : groundi -> groundi -> groundi -> goal

(** Infix syninym for [mulo] *)
val ( * ) : groundi -> groundi -> groundi -> goal

(** Relational division *)
val divo :  groundi -> groundi -> groundi -> goal  

(** Infix syninym for [divo] *)
val ( / ) : groundi -> groundi -> groundi -> goal


(** Comparisons *)
val leo : groundi -> groundi -> LBool.groundi -> goal
val geo : groundi -> groundi -> LBool.groundi -> goal
val gto : groundi -> groundi -> LBool.groundi -> goal
val lto : groundi -> groundi -> LBool.groundi -> goal

(** Comparisons as goals *)
val (<=) : groundi -> groundi -> goal
val (>=) : groundi -> groundi -> goal
val (>)  : groundi -> groundi -> goal
val (<)  : groundi -> groundi -> goal



