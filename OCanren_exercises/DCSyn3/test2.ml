(** This file tests ... *)

open OCanren;;
open OCanren.Std;;
open Dcsyn3;;

(* test that the standard List.assoco only finds the first match *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:5 @@
  run q (fun q -> ocanren {List.assoco "x" state4 q}) project
