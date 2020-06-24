open GT;;
open Printf;;

(* rename Stdlib.List to L, 
   since OCanren.Std exports:
   List
   Nat
   etc. *)
module L = List;; 

open OCanren;;
open OCanren.Std;;

(* relational min.max for nats *)
let minmaxo a b min max =
  let open Nat in
  ocanren {
    min == a & max == b & a <= b |
    min == b & max == a & a > b
  }
;;

(* s is the smallest element in the non-empty list l, 
   removing the former from the latter gives l' *)

let rec smallesto l s l' =
  ocanren {
    l == [s] & l' == [] |
    fresh h, t, s', t', max in
       l' == max :: t' &
       l == h :: t &
       minmaxo h s' s max &
       smallesto t s' t'
  }
;;

(* convert a logic int list to a functional int list *)
let tofun_int_list = List.to_list Nat.to_int;;
(* convert a functional int list to a logic int list *)
let fromfun_int_list = nat_list;; (* OCanren.nat_list *)

(* project a logic pair int * (int list) back to functional domain *)
let liil_pair_prj pr = Nat.to_int (fst pr), tofun_int_list (snd pr);;

(* wraping samllesto to interface with functional data *)
(* Using Core.qr for two logical parameters *)
let smallest l = liil_pair_prj @@ Stream.hd @@
  run qr
    (* the goal, which returns reified results *)
    (fun s l' -> smallesto (fromfun_int_list l) s l')
    (* the reified result handler. For the class of 
       reified results, see Logic.mli *)
    (fun ss l's -> ss#prj, l's#prj)

let _ = let pr = smallest [4;3;2;1;5;6;7;8] in
  Printf.printf "The smallest is %s and the remainder is: %s\n%!"
     (show(GT.int) (fst pr)) (show(GT.list) (show(GT.int)) (snd pr));; 


