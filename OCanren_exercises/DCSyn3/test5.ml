(** This file tests interpreters with bound *)

open OCanren;;
module L = Stdlib.List;;
open OCanren.Std;;
open Dcsyn3;;
open InterpSZ;;
open InterpSZ.NoLet;;
open TwoBit;;

@type specsz = (Nat.ground, Spec.ground) Pair.ground with show;;

(** forward run of eval_imp *)
module Test1 = struct
  let specs =  
    Stream.take ~n:41  @@ 
    run q (fun q -> ocanren {fresh prog, vs, sts,res,sz in
       prog == Brh(Var "x", Brh(Arr("y", Var "x"), Con c1, Con c2), Con c0)
       & Expr.free_var prog vs
       & Expr.var_state vs sts
       & eval_imp sts prog res sz
       & q == (sz, sts, res)}) project;;
  
  let _ =
    L.iter (fun x -> print_string @@ GT.show(specsz) x;print_newline()) specs;;
end;;

(** forward run of eval_sig *)
module Test2 = struct
  let specs =  
    Stream.take ~n:41  @@ 
    run q (fun q -> ocanren {fresh prog, vs, sts,res,sz in
       prog == Mux(Port "x", Mux(Slice(Port "y", Port "x"), Src c1, Src c2), Src c0)
       & Signal.free_var prog vs
       & Signal.var_state vs sts
       & eval_sig sts prog res sz
       & q == (sz, sts, res)}) project;;
  
  let _ =
    L.iter (fun x -> print_string @@ GT.show(specsz) x;print_newline()) specs;;
end;;
