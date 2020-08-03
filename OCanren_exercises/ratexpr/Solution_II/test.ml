(** Below are some tests *)

open Logic;;
open Core;;
open Ratexpr;;
open LoRat;;
open Typop;;


(** Find expr (sum only) that normalizes to 1/3.
    This is a (systematic) generate-and-test process. *)
let _ = 
  List.iter (fun x -> print_string @@ (GT.show(ground)) x; print_newline()) @@
  RStream.take ~n:1 @@ 
  run q (fun q ->  ocanren {eval q (Num (LPair.pair 1 3))}) project



(*(fun q -> q#reify(reify));;*)


