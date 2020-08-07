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
  val evalb : groundi -> groundi -> goal;;
  module Prj : sig
    open LNat;;
    val logic_to_ground : logic * logic -> ground * ground;;
  end;;
end = struct
  
  module Prj = struct
    let logic_to_ground = fun (a,b) ->
      LoNat.Prj.logic_to_ground a, LoNat.Prj.logic_to_ground b;;
  end;;


  (** Conjuncts reordered from [simplify]. ( * ) is a more efficient generator than
      [gcd] when used backward. The relative order between Two ( * ) is subtle, 
      taking into account the generative behavior of ( < ) where the smaller is always 
      concrete. 
      The performance inhibitor is that [simplify] has two similar clauses for
      [a > b] and [b > a] resp. When used backward, only one clause will do useful 
      work and the other two will only waste time to do useless computations.
      So a way to improve is to remove the brach for [b < a] and enforce [a <= b]
      from the context. *)
  let simplify a b a' b'=
    let open LNat in let open LoNat in
    conde [
      (?& [a === b ; a' === one ; b' === one]);
      (?& [b < a ; Fresh.one (fun q -> (?& [( * ) q b' b ; ( * ) q a' a ; gcd a b q ]))]);
      (?& [a < b ; Fresh.one (fun q -> (?& [( * ) q a' a ; ( * ) q b' b ; gcd b a q ]))])];;

  (** a/b + a'/b' = c/d where c/d is in normal form  *)
  let  radd  a b a' b' c d =
    let open LNat in
    ocanren {
      b == b'  & { fresh k in ( + ) a a' k & simplify k b c d }
  |   b =/= b' & { fresh  ab', a'b, nu, bb' in
        ( * ) a   b'  ab'         
        & ( * ) a'  b   a'b         
        & ( * ) b   b'  bb'         
        & ( + ) nu  a'b ab'          
        & simplify nu bb' c d' } };;


  (** same as [eval] but uses [simplify] *)
  let rec eval ex no =
    let open Inj in  let open LNat in
    ocanren {
      {fresh a, b, a', b' in                              
         ex == Num (a, b)
       & no == Num (a', b')
       & simplify a b a' b' }
     | 
      {fresh ea, eb, na, nb, a, b, a', b',               
             ab', a'b, nu, bb', nu', bb'' in
         ex == Sum (ea, eb)       
       & eval ea na              
       & eval eb nb              
       & na == Num (a, b)          
       & nb == Num (a', b')        
       & ( * ) a   b'  ab'         
       & ( * ) a'  b   a'b         
       & ( * ) b   b'  bb'         
       & ( + ) ab' a'b nu          
       & simplify nu bb' nu' bb'' 
       & no == Num (nu', bb'') }
     |
      {fresh ea, eb, na, nb, a, b, a', b',                
             ab', a'b, nu, bb', nu', bb'' in
         ex == Subt (ea, eb)       
       & eval ea na              
       & eval eb nb              
       & na == Num (a, b)          
       & nb == Num (a', b')        
       & ( * ) a   b'  ab'         
       & ( * ) a'  b   a'b         
       & ( * ) b   b'  bb'         
       & ( + ) nu  a'b ab'          
       & simplify nu bb' nu' bb'' 
       & no == Num (nu', bb'') }
     |
      {fresh ea, eb, na, nb, a, b, a', b',                
             aa', bb', s1, s2 in
         ex == Prod (ea, eb)
       & eval ea na
       & eval eb nb
       & na == Num (a, b)
       & nb == Num (a', b')
       & ( * ) a  a' aa'
       & ( * ) b  b' bb'
       & simplify aa' bb' s1 s2
       & no == Num (s1, s2) } };;


  (** Similar to  [eval] but adds bounds for numerator and denominator.  *)
  let rec evalb ex no =
    let open Inj in  let open LNat in let bound = OCanren.Std.nat 20 in
    ocanren {
      {fresh a, b, a', b' in                              
         ex == Num (a, b)
       & a <= bound & b <= bound   (* forward: check (<=) and compute (simplify) *)
       & no == Num (a', b')
       & simplify a b a' b' }    (* backward: generate (<=) and test (simplify) *)
     | 
      {fresh ea, eb, na, nb, a, b, a', b', c, d in
         ex == Sum (ea, eb)
       & na == Num (a, b)          
       & nb == Num (a', b')    
       & a <= bound & b <= bound   
       & a' <= bound & b' <= bound
       & evalb ea na & evalb eb nb
       & radd a b a' b' c d
       & no == Num (c, d) }
     |
      {fresh ea, eb, na, nb, a, b, a', b',                
             ab', a'b, nu, bb', nu', bb'' in
         ex == Subt (ea, eb)       
       & na == Num (a, b)          
       & nb == Num (a', b')
       & a <= bound & b <= bound   
       & a' <= bound & b' <= bound
       & evalb ea na              
       & evalb eb nb 
       & ( * ) a   b'  ab'         
       & ( * ) a'  b   a'b         
       & ( * ) b   b'  bb'         
       & ( + ) nu  a'b ab'          
       & simplify nu bb' nu' bb'' 
       & no == Num (nu', bb'')  }
     |
      {fresh ea, eb, na, nb, a, b, a', b',                
             aa', bb', s1, s2 in
         ex == Prod (ea, eb)
       & na == Num (a, b)
       & nb == Num (a', b')
       & a <= bound & b <= bound   
       & a' <= bound & b' <= bound
       & evalb ea na
       & evalb eb nb
       & ( * ) a  a' aa'
       & ( * ) b  b' bb'
       & simplify aa' bb' s1 s2
       & no == Num (s1, s2) } };;


end;;




(******************************************************************************************)
