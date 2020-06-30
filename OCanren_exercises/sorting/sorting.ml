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


(* In OCanren, data (e.g., nat,list) resides in four levels: 

   GT-level,
   =============
   ground-level,
   =============
   logic-level,
   =============
   groundi-level
   =============

   Functional programming is done at GT-level, and 
   logic programming is done at groundi-level. Libraries
   provide routines to convert data from GT-level to 
   groundi-level via ground-level and logic-level.
*)
(* The type error massages could be helpful for debugging *)

(* Raise an int list from ground-level to GT-level *)
(* Nat.to_int    : Nat.ground -> int *)
(* List.to_list  : ('a -> 'b) -> 'a List.ground -> 'b GT.list *)
(* -             : Nat.ground List.ground -> int GT.list *)
let tofun_int_list = List.to_list Nat.to_int;;


(* Push an int list from GT-level to groundi-level *)
let fromfun_int_list = nat_list;; (* OCanren.nat_list *)


(* wrapping samllesto to interface with GT-level data *)
(* Using Core.qr for two logical parameters *)
let smallest l =
  (* Raise an int * (int list) pair from ground-level to GT-level *)
  let raise pr = Nat.to_int (fst pr), tofun_int_list (snd pr) in
  raise @@ Stream.hd @@
  run qr
    (* the goal, which returns reified results *)
    (fun s l' -> smallesto (fromfun_int_list l) s l')
    (* the reified result handler. For the class of 
       reified results, see Logic.mli *)
    (fun ss l's -> ss#prj, l's#prj)
    

let _ = let pr = smallest [4;3;2;1;5;6;7;8] in
  Printf.printf "The smallest is %s and the remainder is: %s\n%!"
    (show(GT.int) (fst pr)) (show(GT.list) (show(GT.int)) (snd pr));;


let smallest' l' = 
  (* raise int list * int from ground-level to GT-level *)
  let raise = fun ilst, it ->  tofun_int_list ilst, Nat.to_int it in
  L.map raise  @@  Stream.take @@
  if l' = [] then Stdlib.raise (Invalid_argument "Empty list for samllest'.")
  else
    run qr (fun l s -> smallesto l s (fromfun_int_list l'))
      (fun ls ss -> ls#prj,ss#prj)


let _ =
  let res = smallest' [5;6;4;7;8] in
  (* to-string printer of int GT.list * int *)
  let sprinter = fun ilst,it ->
    (show(GT.list) string_of_int ilst) ^ ", " ^ (show(GT.int) it) ^ "\n"
  in
  L.iter print_string (L.map sprinter res);; 


let _ = try
    begin
      let res = smallest' [] in
      Stdlib.ignore res
    end with Invalid_argument s -> print_string s; print_newline ();; 


(* use the empty list as input *)
let smallest'e =
  begin
    Stream.take @@
    run qr (fun l s -> smallesto l s (fromfun_int_list []))
      (fun ls ss -> ls#reify(List.reify Nat.reify),ss#reify(Nat.reify))
  end;;


let module Smallest'e = struct
  @type t = (Nat.logic List.logic * Nat.logic) with show;;
end in L.iter print_string @@
(L.map
   (fun dt -> sprintf "smallest'e is %s\n%!" (show(Smallest'e.t) dt))
   smallest'e);;


let smallest'' =
  Stream.take ~n:100 @@
  run qrs (fun q r s -> smallesto q r s)
    (fun qs rs ss -> qs#reify(List.reify Nat.reify),
                     rs#reify(Nat.reify),
                     ss#reify(List.reify Nat.reify));;


let module Smallest'' = struct
  @type t = (Nat.logic List.logic * Nat.logic * Nat.logic List.logic)
            with show;;
end in L.iter print_string @@
(L.map
   (fun dt -> sprintf "smallest'' is %s\n%!" (show(Smallest''.t) dt))
   smallest'');;


let smallest''' s l' = L.map tofun_int_list @@
  let f = fromfun_int_list and f' = OCanren.Std.nat in 
  Stream.take @@
  run q (fun q -> smallesto q (f' s) (f l')) (fun qs -> qs#prj);;


let sml s l' =
  let l = smallest''' s l' in 
  let module Smallest''' = struct @type t = int GT.list with show;; end in
  let il2str = show(Smallest'''.t) in
  L.iter (fun s -> print_string s;print_newline()) (L.map il2str l);
  printf "Above: samllest''' %d %s\n" s (il2str l')
in
(* below: gives all permutations of 2..3 *)
sml 2 [3];
(* below: gives all permutations of 1..3 *)
sml 1 [2;3];
sml 1 [3;2];
(* below: gives all permutations of 0..3 *)
sml 0 [1;2;3];
sml 0 [1;3;2];
sml 0 [2;1;3];
sml 0 [2;3;1];
sml 0 [3;1;2];
sml 0 [3;2;1];;


(* wrapping minmaxo to interface with GT-level data, 
   The user provides min and max, minmaxr returns a pair (a, b) *)
(* Use OCanren.Std.nat to push int from GT-level to groundl-level *)
let minmaxr min max =
  (* Raise the (int * int) part of (int * int) list from 
     ground-level to GT-level; the enclosing list itself
     is already in GT-level *)
  let raise pl =
    (* Raise an (int * int) from ground-level to GT-level *)
    let raise' (ft, st) = (Nat.to_int ft), (Nat.to_int st) in
    L.map raise' pl in
  raise @@ Stream.take ~n:2 @@
  run qr (fun a b -> minmaxo  a  b (nat min) (nat max))
    (fun ast bs -> ast#prj, bs#prj);;


let _ = let mi,mx = 2,3 in
  let lp = minmaxr mi mx in
  Printf.printf "Each of the following pairs has min %d and max %d:\n%s\n%!" 
    mi mx (show(GT.list) (fun x,y -> Printf.sprintf "(%d,%d)" x y) lp);;


(* forward *)
let rec sorto x y =
  ocanren {
    x == [] & y == [] |
    fresh s, xs, xs' in
     smallesto x s xs &
        y == s :: xs' &
        sorto xs xs' }


(* backward *)
let rec sorto' x y =
  ocanren {
    x == [] & y == [] |
    fresh s, xs, xs' in
     y == s :: xs' &
     sorto' xs xs'  &
     smallesto x s xs   }


(* sort GT-level nat list l *)
let sort l = tofun_int_list @@ Stream.hd @@ run q
    (fun q -> sorto (fromfun_int_list l) q) (fun qs -> qs#prj);;


let _ = let ls' =  [0;1;9;2;8;3;7;4;6;5] in
  let ls = sort ls' and shil x = (show(GT.list) (show(GT.int)) x) in
  Printf.printf "%s is sorted into %s.\n%!"
    (shil ls') (shil ls);;


let rec factorial n = if n = 0 then 1
  else if n < 0 then Stdlib.raise @@ Invalid_argument "factorial"
      else n * factorial (n - 1);; 


let perm l = L.map tofun_int_list @@ Stream.take @@ run q
    (fun q -> sorto' q (fromfun_int_list l)) (fun qs -> qs#prj);;


(* smallesto is the core of permutation *)
let module Perm  = struct @type t = int GT.list with show;; end
in let ls =  [0;1;2] in
let lls = perm ls in
L.iter (fun s -> print_string s;print_newline()) (L.map (show(Perm.t)) lls);
Printf.printf
  "%s is permutated into %d lists, but it should have %d permutations:\n"
  (show(Perm.t) ls) (L.length lls) (factorial @@ L.length ls);
let ls =  [2;1;0] in
let lls = perm ls in
L.iter (fun s -> print_string s;print_newline()) (L.map (show(Perm.t)) lls);
Printf.printf
  "%s is permutated into %d lists, but it should have %d permutations:\n"
  (show(Perm.t) ls) (L.length lls) (factorial @@ L.length ls);;

