open GT;;
open OCanren;;
open OCanren.Std;;



@type mine = A | B with show;;



let p = 0.2 and r = 0.50 and x = 100.0 ;;
let q = 0.3 and s = 0.33 and y = 120.0;;

(* Use rational numbers instead of floating point *)






let rec expectation plan =
  match plan with
  | [] -> 0.0
  | []



(* Start in A : 
  one possibility : rx -> r(1-r)x -> ...  *)
