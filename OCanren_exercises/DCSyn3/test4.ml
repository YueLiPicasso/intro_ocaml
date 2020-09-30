(** This file tests flowchart sythesis *)
open OCanren;;
module L = List ;;
open OCanren.Std;;
open Dcsyn3;;
open Interp;;
open Interp.NoLet;;
open TwoBit;;

@type svp = State.ground * Value.ground with show;;
(* produce all I/O pairs for a given imperative program *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(svp) x;print_newline())
  @@ Stream.take  @@ 
  run q (fun q -> ocanren {fresh prog, vs, sts,res in
    prog == Brh(Var "x", Brh(Arr("y", Var "x"), Con c1, Con c2), Con c0)
    & Expr.free_var prog vs
    & Expr.var_state vs sts
    & eval_imp sts prog res
    & q == (sts, res)}) project;;

let specs : Spec.ground GT.list =  
  Stream.take  @@ 
  run q (fun q -> ocanren {fresh prog, vs, sts,res in
    prog == Brh(Var "x", Brh(Arr("y", Var "x"), Con c1, Con c2), Con c0)
    & Expr.free_var prog vs
    & Expr.var_state vs sts
    & eval_imp sts prog res
    & q == (sts, res)}) project;;

let _ = Printf.printf "Synthesizing from %d input-output pairs...\n\!" (L.length specs) ;;

let specsi : Specs.groundi = Specs.grd2ijd specs;;
(*
let _ =
  L.iter (fun x -> print_string @@ GT.show(Signal.logic) x;print_newline())
  @@ Stream.take ~n:1 @@
  run q (fun q -> ocanren {syn specsi q}) (fun q -> q#reify(Signal.reify))

*)

