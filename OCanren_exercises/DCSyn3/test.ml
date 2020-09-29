(** This file tests the array accessor, 
    the imperative language interpreter and 
    the filtero utility from OCanren's LList library *)
(** IMPORTANT: make sure that the four bit version of constant and 16-cell version
    of array is used, otherwise the code won't type check.  *)
open OCanren;;
module L = List ;;
open OCanren.Std;;
open Dcsyn3;;
open Dcsyn3.Interp;;

(* test array access *)

let _ = 
  L.iter (fun x -> print_string @@ GT.show(Constant.ground) x;print_newline())
  @@ Stream.take ~n:3 @@ 
  run q (fun q -> ocanren {ArrayAccess.rel c1 array1 q}) project;;


let _ = 
  L.iter (fun x -> print_string @@ GT.show(Array.logic) x;print_newline())
    @@  Stream.take ~n:3 @@ 
  run q (fun q -> ocanren {ArrayAccess.rel c1 q c1}) (fun q -> q#reify(Array.reify));;

(* test eval_imp *)

(* eval constant *)

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {fresh r in eval_imp r (Con c1) q}) project;;

(* eval variable *)

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Var "x") q}) project;;


let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Var "y") q}) project;;


(* eval array *)

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Arr ("y", Con c1)) q}) project;;


let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Arr ("y", Var "x")) q}) project;;


let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Arr ("y", Arr ("y", Var "x"))) q}) project;;


let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Arr ("y", Arr ("y", Arr ("y", Var "x")))) q}) project;;
(* potential optimization: store the value of "y" to avoid repeated lookup *)

(* invalid array access *)

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Arr ("x", Con c1)) q}) project;;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Arr ("y", Var "y")) q}) project;;


(* eval branch *)

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Brh(Con c0, Con c0, Con c1)) q}) project;;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Brh(Con c1, Con c0, Con c1)) q}) project;;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Brh(Brh(Arr("y", Con c0),Con c1,Con c9), Con c0, Con c1)) q}) project;;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Brh(Con c1,Brh(Arr("y", Con c0), Con c9, Con c1), Con c0)) q}) project;;


(* given a state and a result, synthesis programs *)

let _ =
  L.iter (fun x -> print_string @@ GT.show(Expr.logic) x;print_newline())
  @@ Stream.take ~n:5 @@ 
  run q (fun q -> ocanren {eval_imp state1 q (Conv c1)}) (fun q -> q#reify(Expr.reify));;

(* given two  state-result pairs, synthesis programs *)

let _ =  print_newline();;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Expr.logic) x;print_newline())
  @@ Stream.take ~n:5 @@
  run q (fun q -> ocanren {eval_imp state1 q (Conv c1) & eval_imp state2 q (Conv c2)}) (fun q -> q#reify(Expr.reify));;

(* given three  state-result pairs, synthesis programs *)

let _ =  print_newline();;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Expr.logic) x;print_newline())
  @@ Stream.take ~n:5 @@
  run q (fun q -> ocanren {eval_imp state2 q (Conv c7)
                           & eval_imp state2b q (Conv c6)
                           & eval_imp state2c q (Conv c4)}) (fun q -> q#reify(Expr.reify));;

(*
(* given three  state-result pairs, synthesis programs : hard challenge, processs killed; answer
may not exist though *)

let _ =  print_newline();;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Expr.logic) x;print_newline())
  @@ Stream.take ~n:1 @@
  run q (fun q -> ocanren {eval_imp state2 q (Conv c1) & eval_imp state2b q (Conv c2) & eval_imp state2c q (Conv c3)}) (fun q -> q#reify(Expr.reify));;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Expr.logic) x;print_newline())
  @@ Stream.take ~n:1 @@
  run q (fun q -> ocanren {eval_imp state1 q (Conv c5) & eval_imp state2 q (Conv c10) & eval_imp state3 q (Conv c15)}) (fun q -> q#reify(Expr.reify));;
*)


(* test filtero *)

let sunit_test : StateUnit.groundi -> LBool.groundi -> goal =
  fun st bl ->
  ocanren {
    {fresh v in st == ("x", v) & bl == LBool.falso}
  | {fresh k,v in st == (k, v) & k =/= "x" & bl == LBool.truo}
  };;

let _ =  print_newline();;

let _ =
  L.iter (fun x -> print_string @@ GT.show(State.ground) x;print_newline())
  @@  Stream.take ~n:3 @@
  run q (fun q -> List.filtero sunit_test state1 q) project;;

