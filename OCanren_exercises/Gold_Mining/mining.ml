open GT;;
open OCanren;;
open OCanren.Std;;

(* positive rational numbers *)
(* a/b  is representedby a pair (a, b) *)

module Rat = LRational;;



@type mine = A | B with show;;

let p =  Rat.of_int_ratio (3,5);; 
let q = Rat.inj;;

(*

let p = 0.2 and r = 0.50 and x = 100.0 ;;
let q = 0.3 and s = 0.33 and y = 120.0;;

(* Use rational numbers instead of floating point *)

 let rec expectation plan =
  match plan with
  | [] -> 0.0
  | []

*)
