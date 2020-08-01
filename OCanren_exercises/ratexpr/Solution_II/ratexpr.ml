open Logic;;
open Core;;

@type 'a logic' = 'a logic
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(******************************************************************************************)

@type ('nat, 'self) rat_expr =
     Num of 'nat * 'nat             
   | Sum of 'self * 'self           
   | Subt of 'self * 'self          
   | Prod of 'self * 'self          
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

module X = struct
  @type ('a,'b) t = ('a,'b) rat_expr
   with show, html, eq, compare, foldl, foldr, gmap, fmt;;
  let fmap = fun x y z -> GT.gmap(t) x y z;;
end;;

include X;;

module F = Fmap2(X);;
 
@type ground = (LNat.ground, ground) t
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

@type logic = (LNat.logic, logic) t logic'
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

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

@type frat = (GT.int, frat) t
 with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

(******************************************************************************************)

module Inj : sig
  val num  : LNat.groundi * LNat.groundi -> groundi;; 
  val sum  : groundi * groundi -> groundi;;
  val subt : groundi * groundi -> groundi;;
  val prod : groundi * groundi -> groundi;;
  val reify : VarEnv.t -> groundi -> logic;;
end = struct
  let num  (x, y) = inj @@ F.distrib (Num  (x, y)) 
  and sum  (x, y) = inj @@ F.distrib (Sum  (x, y))
  and subt (x, y) = inj @@ F.distrib (Subt (x, y))
  and prod (x, y) = inj @@ F.distrib (Prod (x, y));;

  (**
     val reify :
      (VarEnv.t -> ('a, 'b) Logic.injected -> 'b) ->
      (VarEnv.t -> ('c, 'd) Logic.injected -> 'd) ->
      VarEnv.t ->
      (('a, 'c) X.t, ('b, 'd) X.t Logic.logic) Logic.injected ->
      ('b, 'd) X.t Logic.logic *)
  let rec reify = fun env n -> F.reify (LNat.reify) reify env n;;
end;;

(******************************************************************************************)

module GNat : sig
  val ( = )  : LNat.ground -> LNat.ground -> GT.bool;;
  val ( < )  : LNat.ground -> LNat.ground -> GT.bool;;
  val ( <= ) : LNat.ground -> LNat.ground -> GT.bool;;
  val ( - )  : LNat.ground -> LNat.ground -> LNat.ground;;
  val ( + )  : LNat.ground -> LNat.ground -> LNat.ground;;
  val ( * )  : LNat.ground -> LNat.ground -> LNat.ground;;
  val ( / )  : LNat.ground -> LNat.ground -> LNat.ground * LNat.ground;;
  val gcd    : LNat.ground -> LNat.ground -> LNat.ground;;
end = struct
  
  (** equallity *)
  let rec nat_eq a b =
    match a, b with
      LNat.O , LNat.O -> true
    | LNat.S a', LNat.S b' -> nat_eq a' b'
    | _ -> false;;

  (** less than *)
  let rec nat_lt a b =
    match a,b with
      LNat.O , LNat.S _ -> true
    | LNat.S a', LNat.S b' -> nat_lt a' b'
    | _ -> false;;

  (** less than or equal *)
  let rec nat_le a b =
    match a,b with
      LNat.O , LNat.O -> true
    | LNat.O , LNat.S _ -> true
    | LNat.S a', LNat.S b' -> nat_le a' b'
    | _ -> false;;

  (** Subtracting a number greater than self is forbidden *)
  let rec minus a b =
    match a,b with
      _ , LNat.O -> a
    | LNat.O, _ -> raise (Invalid_argument "subtracting (S _) from O")
    | LNat.S a' , LNat.S b' -> minus a' b';;
  
  let ( = ) = nat_eq
  and ( < ) = nat_lt
  and ( <= ) = nat_le
  and ( - ) = minus;;
  
  (** Division helper for [a / b] with quotient register c.
      The first argument also serves as the remainder register. 
      It returns the (quotient, remainder) pair.
      There is protection against division by zero. *)
  let rec div_acc a b c =
    if  b = LNat.O then raise Division_by_zero  
    else if a < b then (c, a)
    else if a = b then (LNat.S c, LNat.O)
    else let dif = a - b in
      if dif = b then (LNat.S (LNat.S c), LNat.O)
      else if dif < b then (LNat.S c, dif)
      else div_acc dif b (LNat.S c);;

  (** Division *)
  let ( / ) = fun x y -> div_acc x y LNat.O;;

  (** Euclidean Algorithm *)
  let rec gcd a b =
    if a = b then b
    else if a < b then gcd b a 
    else let (q,r) = a / b in
      if r = LNat.O then b else gcd b r;;

  let rec add a b =
    match a with
    | LNat.O -> b
    | LNat.S a' -> LNat.S (add a' b);;

  let ( + ) = add;;

  let rec mult a b =
    match a with
      LNat.O  -> LNat.O
    | LNat.S a' ->
      begin
        match b with
          LNat.O -> LNat.O
        | LNat.S _ -> b + (mult a' b)
      end;;

  let ( * ) = mult;;
  
end;;

(******************************************************************************************)

module GRat : sig
  val eval : ground -> ground;;
  val simplify : LNat.ground * LNat.ground -> LNat.ground * LNat.ground;;
  val to_frat : ground -> frat;;
  val of_frat : frat -> ground;;
end = struct
  open GNat;;

  (** The definition of [to_frat] is as if given as:

  let rec to_frat = function
      Num (a,b) -> Num ((LNat.to_int a),(LNat.to_int b))
    | Sum  (e1, e2) -> Sum (to_frat e1, to_frat e2)
    | Subt (e1, e2) -> Subt (to_frat e1, to_frat e2)
    | Prod (e1, e2) -> Prod (to_frat e1, to_frat e2);;
  
      Similar for [of_frat]. *)
  let rec to_frat = fun x -> GT.gmap(t) (LNat.to_int) to_frat x;;
  let rec of_frat = fun x -> GT.gmap(t) (LNat.of_int) of_frat x;;
  
  let simplify = fun (a, b) ->
    let g = gcd a b in
    let (a', _) = a / g and (b', _) = b / g in
    (a',b');;

  (** The GT plugin [eval] could also have been used to define the evaluator, if
  only [eval] had been added to LNat. *)
  let rec analyze = fun e1 e2 ->
    let Num (n1,d1) = eval e1
    and Num (n2,d2) = eval e2
    in n1, d1, n2, d2
  and eval = fun ex ->
    match ex with
      Num (n, d) -> let n',d' = simplify (n, d) in Num (n', d')
    | Sum (ex1, ex2) ->
      let n1, d1, n2, d2 = analyze ex1 ex2 in
      let d = d1 * d2 and n = (n1 * d2) + (n2 * d1) in
      let n', d' = simplify (n, d) in
      Num (n', d')
    | Subt (ex1, ex2) ->
      let n1, d1, n2, d2 = analyze ex1 ex2 in
      let d = d1 * d2 and n = (n1 * d2) - (n2 * d1) in
      let n', d' = simplify (n, d) in
      Num (n', d')
    | Prod (ex1, ex2) ->
      let n1, d1, n2, d2 = analyze ex1 ex2 in
      let d = d1 * d2 and n = n1 * n2 in
      let n', d' = simplify (n, d) in
      Num (n', d');;
end;;

(******************************************************************************************)

module LoNat : sig
  open LNat;;
  val divisible_by : groundi -> groundi -> goal;;
  val remainder    : groundi -> groundi -> groundi -> goal;;
  val gcd          : groundi -> groundi -> groundi -> goal;;
  module Prj : sig
    val logic_to_ground : logic -> ground;;
  end;;
end = struct
  open LNat;;

  (** In a similar way we can write a filter to separate all reified data into
      two groups : those that contain free variables which cannot be converted
      to ground values, and those that do not contain free variables and can thus
      be converted to free variables. Then different printers can be applied to 
      get the most human readble display of the result. *)
  module Prj = struct
    let rec logic_to_ground = function
      | Var _ -> raise Not_a_value
      | Value a ->
        begin
          match a with
          | O -> O
          | S b -> S (logic_to_ground b)
        end;;
  end;;

  (** From the second clause we can infer that a =/= zero and that a >= b *)
  let rec divisible_by a b =
    conde [(?& [a === zero ; b =/= zero]); (** This setup for b is a must *)
           (?& [b =/= zero ; Fresh.one (fun c -> addo c b a &&& divisible_by c b)])];; 
  
  let remainder a b r =
    conde [
      (?& [divisible_by a b ; r === zero]);
      (?& [r =/= zero ; r < b ; Fresh.one (fun m -> addo m r a &&& divisible_by m b) ])];;
  
  let rec gcd a b c =
    conde [(?& [b <= a ; divisible_by a b ; c === b]);
           (?& [b < a ; Fresh.one (fun r -> (?& [remainder a b r; r =/= zero; gcd b r c]))])];;   
end;;

(******************************************************************************************)

module LoRat : sig
  val simplify : LNat.groundi -> LNat.groundi -> LNat.groundi -> LNat.groundi -> goal;;
  val eval : groundi -> groundi -> goal;;
  module Prj : sig
    open LNat;;
    val logic_to_ground : logic * logic -> ground * ground;;
  end;;
end = struct
  
  module Prj = struct
    let logic_to_ground = fun (a,b) ->
      LoNat.Prj.logic_to_ground a, LoNat.Prj.logic_to_ground b;;  
  end;;

  let simplify a b a' b'=
    let open LNat in let open LoNat in
    conde [
      (?& [a === b ; a' === one ; b' === one]);
      (?& [b < a ; Fresh.one (fun q -> (?& [gcd a b q ; ( * ) q a' a ; ( * ) q b' b]))]);
      (?& [a < b ; Fresh.one (fun q -> (?& [gcd b a q ; ( * ) q a' a ; ( * ) q b' b]))])];;

  let rec eval ex no =
    let open Inj in let open LNat in
    conde [
      Fresh.four (fun a b a' b' ->
          ?& [ex === num (a, b) ; no === num (a',b') ; simplify a b a' b']);
      Fresh.two (fun ea eb ->
          ?& [ex === sum (ea, eb) ;
              Fresh.two (fun na nb ->
                  ?& [eval ea na ; eval eb nb;
                      Fresh.four (fun a b a' b' ->
                          ?& [na === num (a,b) ; nb === num (a',b') ;
                              Fresh.four (fun ab' a'b bb' nu ->
                                  ?& [( * ) a b' ab';
                                      ( * ) a' b a'b;
                                      ( * ) b b' bb';
                                      ( + ) ab' a'b nu;
                                      Fresh.two (fun nu' bb'' ->
                                          ?& [simplify nu bb' nu' bb'';
                                             no === num (nu',bb'')])])])])]);
      (?& []);
      (?& [])];;
end;;

(******************************************************************************************)

(** Below are some tests *)
module Tests = struct
  open LoNat;;
  open LoRat;;
  
@type pr = GT.int * GT.int with show;;
@type intl = GT.int GT.list with show;;
@type ipl = (GT.int * GT.int) GT.list with show;; 
(** Mixed free variables and ground values are captured by type [logic] *)
@type lnpl = (LNat.logic * LNat.logic) GT.list with show;;


(** Find expr (sum only) that normalizes to 1/3: this is a generate-and-test process *)
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(logic)) fr; print_newline())
  @@ RStream.take ~n:20 @@ 
run q (fun q -> eval q (num (ocanren{1}, ocanren{3}))) (fun q -> q#reify(reify))

(** simplify 108 / 72 *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:10 @@
  run qr (fun q r -> ocanren { simplify 108 72 q r })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;

(** find numbers that simplify to 3 / 2 *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:11 @@
  run qr (fun q r -> ocanren { LNat.( < ) q 30 & LNat.( < ) r 20 & simplify q r 3 2 })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;


(** compute the gcd of 108 and 72 *)
let _ = 
  print_string @@ GT.show(intl) @@ RStream.take @@
  run q (fun q -> ocanren {gcd 108 72 q})  (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** remainder for 100/33 *)
let _ = 
  print_string @@ GT.show(intl) @@ RStream.take ~n:10 @@
  run q (fun q -> ocanren { remainder 100 33 q})
    (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** remainder for 100/44 *)
let _ = 
  print_string @@ GT.show(intl) @@ RStream.take ~n:10 @@
  run q (fun q -> ocanren { remainder 100 44 q})
    (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** Find q <= 100 and r such that q divides 100 with remainder r *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:50 @@
  run qr (fun q r -> ocanren { remainder 100 q r & LNat.( <= ) q 100 })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;


(** Enumerate pairs [(q,r)] where q is divisible by r *)  
let _ =
  print_string @@ GT.show(lnpl) @@ RStream.take ~n:10 @@
  run qr  (fun q r -> ocanren { divisible_by q r } )
    (fun q r -> q#reify(LNat.reify), r#reify(LNat.reify)) ;
  print_newline ();; 

(** Enumerate pairs [(q,r)] where q is divisible by r, discarding the
    cases where [q = 0] and where [q = r]. *)  
let _ =
  print_string @@ GT.show(ipl) @@ List.map (fun (a,b) -> LNat.to_int a, LNat.to_int b)
   @@ List.map LoRat.Prj.logic_to_ground @@ List.tl @@ List.tl @@ RStream.take ~n:100 @@
  run qr  (fun q r -> ocanren { divisible_by q r } )
    (fun q r -> q#reify(LNat.reify), r#reify(LNat.reify)) ;
  print_newline ();; 

(** Find all divisors of 15 *)
let _ =
  print_string @@ GT.show(intl) @@ RStream.take @@
  run q (fun q -> ocanren { divisible_by 15 q } )
    (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** Find all divisors of 97 (a prime number) *)
let _ =
  print_string @@ GT.show(intl) @@ RStream.take @@
  run q (fun q -> ocanren { divisible_by 97 q } )
    (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** Find all divisors of 85 *)
let _ =
  print_string @@ GT.show(intl) @@ RStream.take @@
  run q (fun q -> ocanren { divisible_by 85 q } )
    (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** Find all divisors of 80 *)
let _ =
  print_string @@ GT.show(intl) @@ RStream.take @@
  run q (fun q -> ocanren { divisible_by 80 q } )
    (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** Enumerate natural numbers divisible by 3 *)
let _ =
  print_string @@ GT.show(intl) @@ RStream.take ~n:10 @@
  run q (fun q -> ocanren { divisible_by q 3 } )
    (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** Evaluate an expression *)
let _ =
  print_string @@ GT.show(frat) @@ GRat.to_frat @@
  GRat.eval @@ GRat.of_frat @@
  Subt (Num (100,100), Prod (Num (3,5), Sum (Sum (Num (3,21), Num (12,14)), Num (3,9))));
  print_newline ();;

(** Evaluate an expression *)
let _ =
  print_string @@ GT.show(frat) @@ GRat.to_frat @@
  GRat.eval @@ GRat.of_frat (Sum (Sum (Num (3,21), Num (12,14)), Num (3,9)));
  print_newline ();;

(** Evaluate an expression *)
let _ =
  print_string @@ GT.show(frat) @@ GRat.to_frat @@
  GRat.eval @@ GRat.of_frat (Num (3,21));
  print_newline ();;

(** compute 4 plus 4 *)
let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.( + ) (LNat.of_int 4) (LNat.of_int 4);
  print_newline ();;

(** compute 4 times 4 *)
let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.( * ) (LNat.of_int 4) (LNat.of_int 4);
  print_newline ();;

(** compute 100 plus 4 *)
let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.( + ) (LNat.of_int 100) (LNat.of_int 4);
  print_newline ();;

(** compute 100 times 4 *)
let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.( * ) (LNat.of_int 100) (LNat.of_int 4);
  print_newline ();;

(** Enumerate all naturals less than or equals 10 *)
let _ =
  let open LNat in 
  print_string @@ GT.show(intl) @@  RStream.take @@
  run q (fun q -> ocanren { q <= 10 } ) (fun x -> to_int @@ project x);
  print_newline ();;

(** Test if 5 = 4 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( = ) (LNat.of_int 5) (LNat.of_int 4);
   print_newline ();;

(** Test if 5 = 5 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( = ) (LNat.of_int 5) (LNat.of_int 5);
   print_newline ();;

(** Test if 4 = 5 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( = ) (LNat.of_int 4) (LNat.of_int 5);
   print_newline ();;

(** Test if 5 < 4 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( < ) (LNat.of_int 5) (LNat.of_int 4);
   print_newline ();;

(** Test if 5 < 5 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( < ) (LNat.of_int 5) (LNat.of_int 5);
   print_newline ();;

(** Test if 4 < 5 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( < ) (LNat.of_int 4) (LNat.of_int 5);
  print_newline ();;

(** Test if 5 <= 4 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( <= ) (LNat.of_int 5) (LNat.of_int 4);
   print_newline ();;

(** Test if 5 <= 5 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( <= ) (LNat.of_int 5) (LNat.of_int 5);
   print_newline ();;

(** Test if 4 <= 5 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( <= ) (LNat.of_int 4) (LNat.of_int 5);
   print_newline ();;

(** compute 4 -5 *)
let _ =
  print_string @@
  (try
    GT.show(GT.int) @@ LNat.to_int @@
    GNat.( - ) (LNat.of_int 4) (LNat.of_int 5)
  with Invalid_argument s -> s );
    print_newline ();;

(** compute 4 -4 *)
let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.( - ) (LNat.of_int 4) (LNat.of_int 4);
  print_newline ();;

(** compute 4 - 2 *)
let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.( - ) (LNat.of_int 4) (LNat.of_int 2);
  print_newline ();;

(** compute 4 / 2 *)
let _ =
  print_string @@ GT.show(pr) @@ 
  (match GNat.( / ) (LNat.of_int 4) (LNat.of_int 2)
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;

(** compute 2 / 4 *)
let _ =
  print_string @@ GT.show(pr) @@ 
  (match GNat.( / ) (LNat.of_int 2) (LNat.of_int 4)
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;

(** compute 10 / 7 *)
let _ =
  print_string @@ GT.show(pr) @@ 
  (match GNat.( / ) (LNat.of_int 10) (LNat.of_int 7)
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;

(** compute 77 / 5 *)
let _ =
  print_string @@ GT.show(pr) @@ 
  (match GNat.( / ) (LNat.of_int 77) (LNat.of_int 5)
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;

(** comute 4 / 0 *)
let _ =
  print_string @@
  ( try
  GT.show(pr) @@ 
  match GNat.( / ) (LNat.of_int 4) (LNat.of_int 0)
  with a,b -> LNat.to_int a, LNat.to_int b with Division_by_zero -> "Division_by_zero");
  print_newline ();;

(** Find gcd *)

let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.gcd (LNat.of_int 144) (LNat.of_int 55);
  print_newline ();;

let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.gcd (LNat.of_int 144) (LNat.of_int 56);
  print_newline ();;

let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.gcd (LNat.of_int 144) (LNat.of_int 57);
  print_newline ();;

let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.gcd (LNat.of_int 144) (LNat.of_int 58);
  print_newline ();;

let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.gcd (LNat.of_int 144) (LNat.of_int 59);
  print_newline ();;

(** Simplify ratios *)

let _ =
  print_string @@ GT.show(pr) @@ 
  (match GRat.simplify ((LNat.of_int 2), (LNat.of_int 4))
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;

let _ =
  print_string @@ GT.show(pr) @@ 
  (match GRat.simplify ((LNat.of_int 18800), (LNat.of_int 1000))
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;

(** Why we need to prefix [int] , [bool] and [show] with [GT] ? 
      Because, e.g,  the [int] as a parameter of [show], as in [show(int)], is 
      not a type expression but an object named [int] which is defined in [GT]. *)


end;;