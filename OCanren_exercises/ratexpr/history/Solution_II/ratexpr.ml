open Logic;;
open Core;;

@type 'a logic' = 'a logic
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(******************************************************************************************)

@type ('a, 'b) rat_expr =
     Num  of 'a             
   | Sum  of 'b           
   | Subt of 'b
   | Prod of 'b
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

module X = struct
  @type ('a,'b) t = ('a,'b) rat_expr
   with show, html, eq, compare, foldl, foldr, gmap, fmt;;
  let fmap = fun x y z -> GT.gmap(t) x y z;;
end;;

include X;;

module F = Fmap2(X);;

@type ground = ((LNat.ground, LNat.ground) LPair.ground, (ground, ground) LPair.ground) t
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

@type logic = ((LNat.logic, LNat.logic) LPair.logic,  (logic, logic) LPair.logic) t logic'
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Take care of the parenthesis discipline when refining [show] *)
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
      method show    = GT.show(logic') (fun l -> 
            let show_a = 
              fun a -> (GT.show(LPair.logic)) (GT.show(LNat.logic)) (GT.show(LNat.logic)) a 
            and show_b = 
              fun b -> (GT.show(LPair.logic)) (this#show) (this#show) b
            in (GT.show(t)) show_a show_b l)
    end
};;

(** [Logic.ml] defines that [('a,'b) injected = 'a]. Where does ['b] go ? *)
type groundi = (ground, logic) injected;;

(******************************************************************************************)

module Typop : sig
  val num  : (LNat.ground, LNat.logic, LNat.ground, LNat.logic) LPair.groundi -> groundi;; 
  val sum  : (ground, logic, ground, logic) LPair.groundi -> groundi;;
  val subt : (ground, logic, ground, logic) LPair.groundi -> groundi;;
  val prod : (ground, logic, ground, logic) LPair.groundi -> groundi;;
  val reify : VarEnv.t -> groundi -> logic;;
end = struct
  let num  p = inj @@ F.distrib (Num  p) 
  and sum  p = inj @@ F.distrib (Sum  p)
  and subt p = inj @@ F.distrib (Subt p)
  and prod p = inj @@ F.distrib (Prod p);;

  (** F.reify : 
      (VarEnv.t -> ('a, 'b) Logic.injected -> 'b) ->
      (VarEnv.t -> ('c, 'd) Logic.injected -> 'd) ->
      VarEnv.t ->
      (('a, 'c) X.t, ('b, 'd) X.t Logic.logic) Logic.injected ->
      ('b, 'd) X.t Logic.logic 

      'a : ground pair of ground nats
      'c : ground pair of ground rat expr
      'b : logic pair of logic nats
      'd : logic pair of logic rat expr  *)
  let rec reify = fun env n ->
    F.reify (LPair.reify LNat.reify LNat.reify) (LPair.reify reify reify) env n;;
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
  val simplify :  LNat.groundi -> LNat.groundi -> LNat.groundi -> LNat.groundi -> goal;;
  val eval' : groundi -> groundi -> goal;;
  val eval'' : groundi -> groundi -> goal;;
  module Prj : sig
    open LNat;;
    val logic_to_ground : (logic, logic) LPair.logic -> (ground, ground) LPair.ground;;
  end;;
end = struct
  
  module Prj = struct
    let logic_to_ground = function
      | Var _ -> raise Not_a_value
      | Value (a,b) -> LoNat.Prj.logic_to_ground a, LoNat.Prj.logic_to_ground b;;  
  end;;

  let simplify a b a' b' =
    let open LNat in let open LoNat in
    conde [
      (?& [a === b ; a' === one ; b' === one]);
      (?& [b < a ; Fresh.one (fun q -> (?& [gcd a b q ; ( * ) q a' a ; ( * ) q b' b]))]);
      (?& [a < b ; Fresh.one (fun q -> (?& [gcd b a q ; ( * ) q a' a ; ( * ) q b' b]))])];;

 
  
let rec eval' ex no =
    let open Typop in let open LNat in let open LPair in 
    conde [
      Fresh.four (fun a b a' b' ->
          ?& [ex === num (pair a b) ; no === num (pair a' b') ; simplify a b a' b']);
      Fresh.two (fun ea eb ->
          ?& [ex === sum (pair ea eb) ;
              Fresh.two (fun na nb ->
                  ?& [Fresh.four (fun a b a' b' ->
                          ?& [na === num (pair a b) ; nb === num (pair a' b') ;
                              Fresh.four (fun ab' a'b bb' nu ->
                                  ?& [( * ) a b' ab';
                                      ( * ) a' b a'b;
                                      ( * ) b b' bb';
                                      ( + ) ab' a'b nu;
                                      Fresh.two (fun nu' bb'' ->
                                          ?& [simplify nu bb' nu' bb'';
                                              no === num (pair nu' bb'')])])]);
                     eval' ea na ; eval' eb nb])])];;

let rec eval'' ex no =
    let open Typop in let open LNat in let open LPair in 
    conde [
      Fresh.four (fun a b a' b' ->
          ?& [ex === num (pair a b) ; no === num (pair a' b') ; simplify a b a' b']);
      Fresh.two (fun ea eb ->
          ?& [ex === sum (pair ea eb) ;
              Fresh.two (fun na nb ->
                  ?& [eval'' ea na ; eval'' eb nb;
                      Fresh.four (fun a b a' b' ->
                          ?& [na === num (pair a b) ; nb === num (pair a' b') ;
                              Fresh.four (fun ab' a'b bb' nu ->
                                  ?& [( * ) a b' ab';
                                      ( * ) a' b a'b;
                                      ( * ) b b' bb';
                                      ( + ) ab' a'b nu;   (* (a/b * a'/b' = nu/bb')*)
                                      Fresh.two (fun nu' bb'' ->
                                          ?& [simplify nu bb' nu' bb'';
                                              no === num (pair nu' bb'')])])])])])];;


  
end;;



(*


*)
