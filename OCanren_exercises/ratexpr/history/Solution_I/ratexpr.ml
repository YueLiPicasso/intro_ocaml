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
  val num  : LNat.groundi -> LNat.groundi -> groundi;; 
  val sum  : groundi -> groundi -> groundi;;
  val subt : groundi -> groundi -> groundi;;
  val prod : groundi -> groundi -> groundi;;
  val reify : VarEnv.t -> groundi -> logic;;
end = struct
  let num  x y = inj @@ F.distrib (Num  (x, y)) 
  and sum  x y = inj @@ F.distrib (Sum  (x, y))
  and subt x y = inj @@ F.distrib (Subt (x, y))
  and prod x y = inj @@ F.distrib (Prod (x, y));;

  let rec reify = fun env n -> F.reify (LNat.reify) reify env n;;
end;;

(******************************************************************************************)

module GNat : sig
  open LNat;;
  val ( = )  : ground -> ground -> GT.bool;;
  val ( < )  : ground -> ground -> GT.bool;;
  val ( <= ) : ground -> ground -> GT.bool;;
  val ( - )  : ground -> ground -> ground;;
  val ( + )  : ground -> ground -> ground;;
  val ( * )  : ground -> ground -> ground;;
  val ( / )  : ground -> ground -> ground * ground;;
  val gcd    : ground -> ground -> ground;;
  val simplify : ground * ground -> ground * ground;;
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

  let simplify = fun (a, b) ->
    let g = gcd a b in
    let (a', _) = a / g and (b', _) = b / g in
    (a',b');;
end;;

(******************************************************************************************)

module GRat : sig
  val eval : ground -> ground;;
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

module LoNat = struct
  open LNat;;
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
  let rec div a b q r =
    ocanren {
      b =/= 0 & a < b  & q == 0 & r == a
    | b =/= 0 & a == b & q == 1 & r == 0
    | b =/= 0 & a > b  & fresh c, q' in ( + ) b c a & ( + ) 1 q' q & div c b q' r };;
  (** Non-commutative relations *)
  module NonCom = struct
    (** not commutative: must be [a >= b]*)
    let rec gcd a b c =
      ocanren { b <= a & fresh q in div a b q 0 & c == b
              | b < a & fresh q, r in div a b q r & r =/= zero & gcd b r c };;
    (** not commutative: must be [a >= b] *)
    let lcm a b c = ocanren { fresh ab, g in ( * ) a b ab & gcd a b g & ( * ) c g ab };;
  end;;
  (** commutative: if [gcd a b c] then [gcd b a c] *)
  let gcd a b c =  let open NonCom in
    ocanren { a == b & c == b & c =/= zero
            | a < b  & gcd b a c 
            | b < a  & gcd a b c };;
  (** commutative: if [lcm a b c] then [lcm b a c] *)
  let lcm a b c =  let open NonCom in
    ocanren { a == b & c == b
            | a < b  & lcm b a c 
            | b < a  & lcm a b c };;
  (** siplify the rational number [a/b] to [a'/b'] *)
  let simplify a b a' b'= 
      ocanren {  b =/= 0 &
      {  a == 0  & a' == 0 & b' == 1
      |  a == b  & a' == 1 & b' == 1
      |  a =/= 0 & a =/= b & fresh cm in div a cm a' 0 & div b cm b' 0 & gcd a b cm } };;
  module AddCases = struct
    (** must be [b == b']. [ed] for Equal Denominator *)
    let  radd_ed  a b a' b' c d = 
      ocanren { b == b'  &  fresh k in ( + ) a a' k & simplify k b c d  };;
    (** [gt] for Greater Than, i.e., b > b'. 
    [dv] for dibisible, i.e., [b] is divisible by [b'] *)
    let  radd_gt_dv  a b a' b' c d = 
      ocanren {
        b >  b' &  fresh q, a'' in
                 div  b  b' q  0
                 & ( * ) q  a' a''
                 & radd_ed a b a'' b c d };;
    (** [gt] for Greater Than, i.e., b > b'.
    [rm] for remainder, meaning that [b/b'] has a non-zero remainder. 
    When only [a,b] are unknown, the search is inefficient; when only [a',b'] are unknown,
    the search is better than the earlier case, because at least b' is bounded by [b > b'].
    When used for subtruction, the best performance is obtained 
    if the second addend [a'/b'] (rather than the first) is left unknown, 
    and the denominator of the first addend is small *)
    let  radd_gt_rm  a b a' b' c d = 
      ocanren {
         b >  b'  & { fresh q, r, lc, k, ka in
                   r =/= 0
                 & div b   b' q  r  
                 & lcm b   b' lc        
                 & div lc  b  k  0
                 & div ka  k  a  0
                 & radd_gt_dv ka lc a' b' c d } };;
  end;;
  let radd a b a' b' c d = let open AddCases in
    ocanren { radd_ed a b a' b' c d
            | radd_gt_dv a b a' b' c d
            | radd_gt_rm a b a' b' c d
            | radd_gt_dv a' b' a b c d
            | radd_gt_rm a' b' a b c d};;
  let rmul a b a' b' c d = ocanren
      { b =/= 0 & b' =/= 0 &
         {  a ==  0 & c  ==  0 & d == 1 
          | a =/= 0 & a' ==  0 & c == 0 & d == 1  
          | a =/= 0 & a' =/= 0 & fresh c', d' in
            div c' a a' 0 & div d' b b' 0 & simplify c' d' c d }};;

end;;

(******************************************************************************************)

  let rec eval ex no = let open LoNat in let open Inj in
    ocanren {
      {fresh a, b, a', b' in                              
         ex == Num (a, b)
       & no == Num (a', b')
       & simplify a b a' b' }
     | 
      {fresh ea, eb, na, nb, a, b, a', b', c, d in
         ex == Sum (ea, eb)
       & no == Num (c, d)
       & eval ea na              
       & eval eb nb              
       & na == Num (a, b)          
       & nb == Num (a', b')        
       & radd a b a' b' c d }
     |
      {fresh ea, eb, na, nb, a, b, a', b', c, d in
         ex == Subt (ea, eb)
       & no == Num (c, d)
       & eval ea na              
       & eval eb nb              
       & na == Num (a, b)          
       & nb == Num (a', b')
       & radd c d a' b' a b }
     |
      {fresh ea, eb, na, nb, a, b, a', b', c, d in
         ex == Prod (ea, eb)
       & no == Num (c, d)
       & eval ea na
       & eval eb nb
       & na == Num (a, b)
       & nb == Num (a', b')
       & rmul a b a' b' c d } };;



