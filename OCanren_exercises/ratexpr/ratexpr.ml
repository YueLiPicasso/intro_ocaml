open Logic;;
open Core;;

(* Provide an alias for the name from the module Logic *)
@type 'a logic' = 'a logic with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(* Type for arithmetical expressions 
   of positive rational numbers *)
@type ('nat, 'self) rat_expr =
     Num of 'nat * 'nat              (* A positive rational number *)
   | Sum of 'self * 'self            (* Sum of two rat expr *)
   | Subt of 'self * 'self           (* subtraction between two rat expr *)
   | Prod of 'self * 'self           (* Product of two rat expr *)
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

module X = struct
  @type ('a,'b) t = ('a,'b) rat_expr with  show, html, eq, compare, foldl, foldr, gmap, fmt;;
  let fmap = fun x y z -> GT.gmap(t) x y z;;
end;;

include X;;

module F = Fmap2(X);;
 
@type ground = (LNat.ground, ground) t with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

@type logic = (LNat.logic, logic) t logic' with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

let logic = {
  logic with
  GT.plugins =
    object(this)
      method compare = logic.GT.plugins#compare
      method gmap    = logic.GT.plugins#gmap
      method eq      = logic.GT.plugins#eq
      method foldl   = logic.GT.plugins#foldl
      method foldr   = logic.GT.plugins#foldr
      method html    = logic.GT.plugins#html
      method fmt     = logic.GT.plugins#fmt
      method show    = GT.show(logic')
          (fun l -> GT.show(t) (GT.show(LNat.logic)) this#show l)
    end
};;

type groundi = (ground, logic) injected;;

module Inj : sig
  val num  : LNat.groundi * LNat.groundi -> groundi;;
  val sum  : groundi * groundi -> groundi;;
  val subt : groundi * groundi -> groundi;;
  val prod : groundi * groundi -> groundi;;
end = struct
  let num  (x, y) = inj @@ F.distrib (Num  (x, y))
  and sum  (x, y) = inj @@ F.distrib (Sum  (x, y))
  and subt (x, y) = inj @@ F.distrib (Subt (x, y))
  and prod (x, y) = inj @@ F.distrib (Prod (x, y));;
end;;


module Ground_Nat : sig
  val ( = )  : LNat.ground -> LNat.ground -> GT.bool;;
  val ( < )  : LNat.ground -> LNat.ground -> GT.bool;;
  val ( <= ) : LNat.ground -> LNat.ground -> GT.bool;;
  val ( - )  : LNat.ground -> LNat.ground -> LNat.ground;;
  val ( / )  : LNat.ground -> LNat.ground -> LNat.ground * LNat.ground;; 
end = struct
  
  (* equallity *)
  let rec nat_eq a b =
    match a, b with
      LNat.O , LNat.O -> true
    | LNat.S a', LNat.S b' -> nat_eq a' b'
    | _ -> false;;

  (* less than *)
  let rec nat_lt a b =
    match a,b with
      LNat.O , LNat.S _ -> true
    | LNat.S a', LNat.S b' -> nat_lt a' b'
    | _ -> false;;

  (* less than or equal *)
  let rec nat_le a b =
    match a,b with
      LNat.O , LNat.O -> true
    | LNat.O , LNat.S _ -> true
    | LNat.S a', LNat.S b' -> nat_le a' b'
    | _ -> false;;

  (* subtraction *)
  let rec minus a b =
    match a,b with
      _ , LNat.O -> a
    | LNat.O, _ -> raise (Invalid_argument "subtracting (S _) from O")
    | LNat.S a' , LNat.S b' -> minus a' b';;
  
  let ( = ) = nat_eq
  and ( < ) = nat_lt
  and ( <= ) = nat_le
  and ( - ) = minus;;
  
  (* division a/b with accumulator c; returns (quotient, remainder) *)
  let rec div_acc a b c =
    if  b = LNat.O then raise Division_by_zero
    else if a < b then (c, a)
    else if a = b then (LNat.S c, LNat.O)
    else let dif = a - b in
      if dif = b then (LNat.S (LNat.S c), LNat.O)
      else if dif < b then (LNat.S c, dif)
      else div_acc dif b (LNat.S c);;

  let ( / ) = fun x y -> div_acc x y LNat.O;;

end;;


(* Why we need to prefix 'int' and 'show' with GT? See below. We also need
   GT.bool *)

@type intl = GT.int GT.list with show;;

let _ =
  let open LNat in 
  print_string @@ GT.show(intl) @@  RStream.take @@
  run q (fun q -> ocanren { q <= 10 } ) (fun x -> to_int @@ project x);
  print_newline ();;

let _ =
  print_string @@ GT.show(GT.bool) @@ Ground_Nat.( = ) (LNat.of_int 5) (LNat.of_int 4);
   print_newline ();;

let _ =
  print_string @@ GT.show(GT.bool) @@ Ground_Nat.( = ) (LNat.of_int 5) (LNat.of_int 5);
   print_newline ();;

let _ =
  print_string @@ GT.show(GT.bool) @@ Ground_Nat.( = ) (LNat.of_int 4) (LNat.of_int 5);
   print_newline ();;

let _ =
  print_string @@ GT.show(GT.bool) @@ Ground_Nat.( < ) (LNat.of_int 5) (LNat.of_int 4);
   print_newline ();;

let _ =
  print_string @@ GT.show(GT.bool) @@ Ground_Nat.( < ) (LNat.of_int 5) (LNat.of_int 5);
   print_newline ();;

let _ =
  print_string @@ GT.show(GT.bool) @@ Ground_Nat.( < ) (LNat.of_int 4) (LNat.of_int 5);
  print_newline ();;

let _ =
  print_string @@ GT.show(GT.bool) @@ Ground_Nat.( <= ) (LNat.of_int 5) (LNat.of_int 4);
   print_newline ();;

let _ =
  print_string @@ GT.show(GT.bool) @@ Ground_Nat.( <= ) (LNat.of_int 5) (LNat.of_int 5);
   print_newline ();;

let _ =
  print_string @@ GT.show(GT.bool) @@ Ground_Nat.( <= ) (LNat.of_int 4) (LNat.of_int 5);
   print_newline ();;

let _ =
  print_string @@
  (try
    GT.show(GT.int) @@ LNat.to_int @@
    Ground_Nat.( - ) (LNat.of_int 4) (LNat.of_int 5)
  with Invalid_argument s -> s );
    print_newline ();;

let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  Ground_Nat.( - ) (LNat.of_int 4) (LNat.of_int 4);
  print_newline ();;

let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  Ground_Nat.( - ) (LNat.of_int 4) (LNat.of_int 2);
  print_newline ();;

@type pr = GT.int * GT.int with show;;

let _ =
  print_string @@ GT.show(pr) @@ 
  (match Ground_Nat.( / ) (LNat.of_int 4) (LNat.of_int 2)
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;


let _ =
  print_string @@ GT.show(pr) @@ 
  (match Ground_Nat.( / ) (LNat.of_int 2) (LNat.of_int 4)
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;


let _ =
  print_string @@ GT.show(pr) @@ 
  (match Ground_Nat.( / ) (LNat.of_int 10) (LNat.of_int 7)
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;


let _ =
  print_string @@ GT.show(pr) @@ 
  (match Ground_Nat.( / ) (LNat.of_int 77) (LNat.of_int 5)
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;

let _ =
  print_string @@
  ( try
  GT.show(pr) @@ 
  match Ground_Nat.( / ) (LNat.of_int 4) (LNat.of_int 0)
  with a,b -> LNat.to_int a, LNat.to_int b with Division_by_zero -> "Division_by_zero");
  print_newline ();;

