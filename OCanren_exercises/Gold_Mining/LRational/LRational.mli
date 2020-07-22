open Logic
open Core


(** Type synonym to prevent toplevel [logic] from being hidden *)
@type 'a logic' = 'a logic with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Abstract rational type *)
@type 'a rat = 'a * 'a with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Synonym for  Abstract rational type *)
@type 'a t = 'a rat with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Ground rational *)
@type ground = LNat.ground t with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Logic rational *)
@type logic = LNat.logic t logic' with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Logic injection (for reification) *)
val inj : ground -> logic;;

(** A type synonym for injected rat *)
type groundi = (ground, logic) injected;;

(** Reifier *)
val reify : VarEnv.t -> groundi -> logic;;

(** Shallow non-variable projection *)
val prjc : (int -> LNat.ground GT.list -> LNat.ground) ->
  (int -> ground GT.list -> ground) -> VarEnv.t -> groundi -> ground;;

(** [of_int_ratio n] converts integer pair [(m,n)] into [ground]; negative integers become [O] *)
val of_int_ratio : int * int -> ground;;

(** [to_int_ratio g] converts ground [g] into integer pair *)
val to_int_ratio : ground -> int * int;;

(** Make injected [rat] from ground one *)
val to_rat : ground -> groundi;;

(** Relational multiplication *)
val mulo  : groundi -> groundi -> groundi -> goal;;

(** Infix syninym for [mulo] *)
val ( * ) : groundi -> groundi -> groundi -> goal;;

(** Relational division *)
val divo :  groundi -> groundi -> groundi -> goal;;  

(** Infix syninym for [divo] *)
val ( / ) : groundi -> groundi -> groundi -> goal;;

(** Relational addition *)
val addo  : groundi -> groundi -> groundi -> goal;;

(** Infix syninym for [addo] *)
val ( + ) : groundi -> groundi -> groundi -> goal;;

(*

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


*)
