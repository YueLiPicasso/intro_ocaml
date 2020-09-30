(** This file tests ... *)
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



let aaaa =
   Stream.take  @@ 
  run q (fun q -> ocanren {fresh prog, vs, sts,res in
    prog == Brh(Var "x", Brh(Arr("y", Var "x"), Con c1, Con c2), Con c0)
    & Expr.free_var prog vs
    & Expr.var_state vs sts
    & eval_imp sts prog res
    & q == (sts, res)}) id;;
