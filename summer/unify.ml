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

(* repress links *)

let rec repr t =
  match t.texp with
    Link u -> let v = repr u in t.texp <- Link v; v
  | Desc _ -> t
;;

(* access the representation of a type *)

let desc t =
  match (repr t).texp with
    Link u -> assert false
  | Desc d -> d
;;

exception Unify of texp * texp;;

exception Arity of texp * texp;;

let link t1 t2 = (repr t1).texp <- Link t2;;
(* why no just t1.texp <- Link t2 *)

let rec unify t1 t2 =
  let t1 = repr t1 and t2 = repr t2 in
  if t1 == t2 then () else
    match desc t1, desc t2 with
    | Tvar _, _ ->
      link t1 t2
    | _, Tvar _ ->
      link t2 t1
    | Tcon (g1, l1),Tcon(g2, l2) when g1 = g2 ->
      link t1 t2;
      List.iter2 unify l1 l2
    | _, _ -> raise (Unify (t1, t2))
;;



(* scratch area *)

(*
Compare the following let-bindings:

let tint = texp (Tcon (Tint, []));;
let tint' () =  texp (Tcon (Tint, []));;
let tint'' () = {texp = Desc (Tcon(Tint, [])); mark = 0};;

they all create something like {texp = Desc (Tcon(Tint, [])); mark = 0};;).
Hhowever, all occurrances of tint refer to the same physical object, while
separate calls of  tint'() create  distinct physical records,  all of which 
share the same physical (Tcon (Tint, [])). Moreover, separate calls of tint''()
create distinct phsical records whose texp fields also share the same physical 
(Tcon (Tint, [])). 

*)

tint == tint;; (* true *)
let a = tint and b = tint in a == b;; (* true *)

let tint' () =  texp (Tcon (Tint, []));;
let tint'' () = {texp = Desc (Tcon(Tint, [])); mark = 0};;


let a = tint'();;
let b = tint'();;

a == b;; (* false *)

desc a;; (* - : desc = Tcon (Tint, []) *)
desc b;; (* - : desc = Tcon (Tint, []) *)

desc a == desc b;; (* true *)

let a' = tint'' ();;
let b' = tint'' ();;

desc a';; (* - : desc = Tcon (Tint, []) *)
desc b';; (* - : desc = Tcon (Tint, []) *)

a' == b';; (* false *)

desc a' == desc b';; (* true *)

Tint = Tint;;  (* true *)
Tint == Tint;; (* true *)
Tcon (Tint, []) = Tcon(Tint, []);;  (* true *)
Tcon (Tint, []) == Tcon(Tint, []);; (* false *)
Desc (Tcon (Tint, [])) == Desc (Tcon (Tint, []));; (* false *)
{texp = Desc (Tcon (Tint, [])); mark = 0} == {texp = Desc (Tcon (Tint, [])); mark = 0};;
(* false *)
desc {texp = Desc (Tcon (Tint, [])); mark = 0} == desc {texp = Desc (Tcon (Tint, [])); mark = 0};;
(* false *)

let d1 = Tcon (Tint, []);;
let d2 = Tcon (Tint, []);; 

d1 == d2;; (* false *)

desc (texp d1) == desc (texp d2);; (* false *)

desc (texp d1) == d1;; (* true *)
desc (texp d2) == d2;; (* true *)

[] == [];;     (* true *)
(Tint, []) = (Tint, []);;  (* true *)
(Tint, []) == (Tint, []);; (* false *)
(1,2) == (1,2);;           (* false *)
(* occurrances of pairs refer to distinct physical objects
   regardless of their content *)

tint'() = tint;; (* true *)

tint;;


unify a b;;

a,  b ;;

a == b;;

unify a' b';;

a', b';;

desc (repr a') == desc b';; (* true *)


tint == tint ;; (* true *)
tint'() == tint'();; (* false *)
texp (Tcon(Tint,[])) == texp (Tcon(Tint,[]));; (* false *)


tarrow tint tint == tarrow tint tint;;


let t1 = tarrow tint (tarrow tint tint);;

let v = tvar();;

let t2 = tarrow tint v;;

unify t1 t2;;

t1;;

t2 ;;

v;;

let t = tint;;

unify v t;;

v;;

t;;

(repr v) == (repr t);;

v == repr v;;

t == repr t;;

unify v  (tarrow tint (tarrow tint tint));;

v;;

repr v;;

tarrow tint (tarrow tint tint);;



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
