(** This file tests flowchart sythesis *)
open OCanren;;
module L = List ;;
open OCanren.Std;;
open Dcsyn3;;
open Interp;;
open Interp.NoLet;;
open TwoBit;;

(** compute all possible input/output combinations, viz., I/O pairs, of the given imperative program *)
let specs : Spec.ground GT.list =  
  Stream.take  @@ 
  run q (fun q -> ocanren {fresh prog, vs, sts,res in
    prog == Brh(Var "x", Brh(Arr("y", Var "x"), Con c1, Con c2), Con c0)
    & Expr.free_var prog vs
    & Expr.var_state vs sts
    & eval_imp sts prog res
    & q == (sts, res)}) project;;

(** Print all the I/O pairs *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(Spec.ground) x;print_newline()) specs;;

(** count the number of the I/O pairs *)
let _ = Printf.printf "Synthesizing from %d input-output pairs...\n" (L.length specs) ;;

(** convert the I/O pairs from ground to groundi *)
let specsi : Specs.groundi = Specs.grd2ijd specs;;

(** synthesize a flowchart program that satisfies all the I/O pairs *) 
let _ =
  L.iter (fun x -> print_string @@ GT.show(Signal.logic) x;print_newline())
  @@ Stream.take ~n:1 @@
  run q (fun q -> ocanren {syn specsi q}) (fun q -> q#reify(Signal.reify))



