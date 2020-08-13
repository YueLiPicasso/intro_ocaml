open OCanren;;
module L = List;;
open OCanren.Std;;

(** {1 Types for the imperative language} *)

(** use integer or string as ['a] *)
@type 'a var = 'a with show, gmap, eq, compare;;

@type 'var expr = Low | High | Var of 'var with show, gmap, eq, compare;;

@type ('var, 'expr, 'stat ) stat = If_then_else 'expr * 'stat * 'stat
                                 | ColonEq 'var * 'expr
 with show, gmap, eq, compare;;

@type ('a, 'p) prog = ('a, 'p ) List.t with show, gmap, eq, compare;;


(** {1 Types for the imperative language} *)

