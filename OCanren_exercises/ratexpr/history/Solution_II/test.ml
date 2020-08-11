(** Below are some tests *)

open Logic;;
open Core;;
open Ratexpr;;
open LoRat;;
open Typop;;

@type lnpl = (LNat.logic * LNat.logic) GT.list with show;;
@type ipl = (GT.int * GT.int) GT.list with show;;
@type i3pl = (GT.int * GT.int * GT.int) GT.list with show;;
@type ln4pl = (LNat.logic * LNat.logic * LNat.logic * LNat.logic) GT.list with show;; 
@type intl = GT.int GT.list with show;;
@type tmp = ground GT.list with show;;


let _ = 
  print_string @@ GT.show(tmp) @@ RStream.take ~n:1 @@
  run q (fun q -> ocanren {eval'' (Sum ((Num (pair 1 3)), (Num (4, 5)))) q})
    project;
  print_newline();;


(*
(** Find expr (sum only) that normalizes to 1/3.
    This is a (systematic) generate-and-test process. *)
let _ = 
  List.iter (fun x -> print_string @@ (GT.show(ground)) x; print_newline()) @@
  RStream.take ~n:10 @@ 
  run q (fun q ->  ocanren {eval q (Num (LPair.pair 1 3))}) project;;


(*
(** Test the generative power: fast but not very high quality answers: 
    the last number is always 1 for 10 and 100 answers. *)
let _ =
  let open LNat in  
  print_string @@ GT.show(ln4pl) @@ RStream.take ~n:100 @@
  run qrst (fun a b a' b' -> (?& [b' =/= zero ; b < a ; b' < a'; a' < a ; b' < b]))
    (fun a b a' b' ->
       a #reify(reify) ,
       b #reify(reify) ,
       a'#reify(reify) ,
       b'#reify(reify) );
  print_newline();;
*)

(*





(** find  numbers that simplify to 1/3  *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:10 @@
  run qr (fun q r -> ocanren {simplify q r 1 3} )
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline();;


*)





*)
