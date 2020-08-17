(** A relational translator *)
open OCanren;;
module L = List;;
open OCanren.Std;;


(** shared syntactic categories *)

@type 'a var = 'a with show, gmap, eq, compare;; (** use integer or string as ['a] *)

@type 'var expr = Low | High | Var of 'var with show, gmap, eq, compare;;

(** syntactic categories unique to the imperative language *)

@type ('var, 'expr, 'stat ) stat = If_then_else 'expr * 'stat * 'stat
                                 | Assign 'var * 'expr
 with show, gmap, eq, compare;;

@type ('a, 'p) prog = ('a, 'p ) List.t with show, gmap, eq, compare;;


(** syntactic categories unique to the flowchart language *)

