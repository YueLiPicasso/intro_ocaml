let store = ref None;;
(* store : '_weak1 option ref 
   where '_weak1 is a weak type variable, or 
   weakly polymorphic type variable *)

!store;; (* '_weak1 option *)
store := Some 2; !store;; (* Some 2 : int option*)
store;; (* int option ref *)
(* type error:
   store := Some 'a';; *)

(* A weak type variable is a place holder for 
   a single type that is currently unknown. Once the 
   type is known, it will be systematiclly substituted
   for '_weak.  *)

(* weal type variables allude to single-use polymorphism; 
   generic type variables allude to repeatable polymorphism.
*)

let swap store x = match !store with
  | None -> store := Some x; x
  | Some y -> store := Some x ; y;;


let store = ref None in
let v = swap store 1 in
v, swap store 2;;

store;; swap store 4;;
swap store 5;; !store;;

let store = ref None;; (* store: '_weak option ref*)
store := Some (fun x-> x);store;; (* store : ('_weak1 -> '_weak1) option ref *)
store := Some (fun x -> 5 - x);store;; (* store : (int -> int) option ref *)
store := Some (fun x -> x * 3);store;;
match !store with Some f -> f 4 | None -> 0;; 


let store = ref None;; (* store: '_weak option ref*)
store := Some (fun x-> true);store;; (* store : ('_weak1 -> bool) option ref *)
store := Some (fun x -> 5 < x);store;; (* store : (int -> bool) option ref *)
store := Some (fun x -> 6 >= x);store;;
