(** This file tests ... *)

open OCanren;;
open OCanren.Std;;
open Dcsyn3;;

(* test that the standard List.assoco only finds the first match *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:5 @@
  run q (fun q -> ocanren {List.assoco "x" state4 q}) project;;


(* test eval_sig *)

(* eval_sig on Src *)

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:5 @@
  run q (fun q -> ocanren {fresh s in eval_sig s (Src c13) q}) project;;


(* eval_sig on Port *)

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:5 @@
  run q (fun q -> ocanren {eval_sig state4 (Port "x") q}) project;;


let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:5 @@
  run q (fun q -> ocanren {eval_sig state4 (Port "y") q}) project;;

(* eval_sig on Mux *)

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:5 @@
  run q (fun q -> ocanren {eval_sig state1 (Mux (Src c1, Src c2, Src c3)) q}) project;;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:5 @@
  run q (fun q -> ocanren {eval_sig state1 (Mux (Src c0, Src c2, Src c3)) q}) project;;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:5 @@
  run q (fun q -> ocanren {eval_sig state1 (Mux (Port "x", Src c2, Src c3)) q}) project;;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:5 @@
  run q (fun q -> ocanren {eval_sig state1
                             (Mux (Port "x",
                                   Mux (Port "y", Port "y", Src c5),
                                   Src c3)) q}) project;;

(* eval_sig on Slice *)
let _ =  print_newline();;


let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:5 @@
  run q (fun q -> ocanren {eval_sig state2 (Slice (Port "x", Src c5)) q}) project;;


let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:5 @@
  run q (fun q -> ocanren {eval_sig state2 (Slice (Port "y", Port "x")) q}) project;;


let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:5 @@
  run q (fun q -> ocanren {eval_sig state2 (Slice (
      Mux (Src c0, Src c5, Port "y"),
      Mux (Port "x", Src c2, Src c3))) q}) project;;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:5 @@
  run q (fun q ->
      ocanren {eval_sig state2 (Slice (Port "y", Mux(Src c0, Src c1, Port "y"))) q}) project;;
