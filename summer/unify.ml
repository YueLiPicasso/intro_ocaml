open Syntax;;

type type_symbol = Tarrow | Tint;;

type texp = {mutable texp : node; mutable mark : int}
and node = Desc of desc | Link of texp
and desc = Tvar of int | Tcon of type_symbol * texp list;;

(* auxiliary functions to build types *)

let texp d = {texp = Desc d; mark = 0};; (* create type expression *)

let count = ref 0;; 
 
let tvar () = incr count; texp (Tvar !count);; (* create fresh type variables *)

let tint = texp (Tcon (Tint, []));;

let tarrow t1 t2 = texp (Tcon(Tarrow, [t1;t2]));;

let last_mark = ref 0;;

let marker () = incr last_mark; !last_mark;;

let rec repr t =
  match t.texp with
    Link u -> let v = repr u in t.texp <- Link v; v
  | Desc _ -> t
;;



(* scratch area *)


let exp = {texp = Link
                     {texp = Link
                          {texp = Link{texp = Desc (Tvar 1); mark = 0};
                           mark = 0};
                      mark = 0};
           mark = 0};;

repr exp;;

repr {texp = Link{texp = Desc (Tvar 1); mark = 0}; mark = 0};;

repr {texp = Desc (Tvar 1); mark = 0};;

exp;;

marker ();;

incr count;;

!count;;

ref 1 == ref 1;;

let v1 = tvar ();;

Tvar 6;; (* desc *)

Desc (Tvar 6);; (* node *)

{texp = Desc (Tvar 6); mark = 1};; (* texp *)

Tcon (Tarrow, [{texp = Desc (Tvar 6); mark = 1}]);; (* desc *)

Link {texp = Desc (Tvar 6); mark = 1};; (* node *)

{texp = Link {texp = Desc (Tvar 6); mark = 1}; mark = 2} ;; (* texp *)
