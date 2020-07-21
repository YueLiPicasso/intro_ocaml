open Logic
open Core

@type 'a rat = 'a * 'a with show, html, eq, compare, foldl, foldr, gmap, fmt
@type 'a logic' = 'a logic with show, html, eq, compare, foldl, foldr, gmap, fmt

let logic' = logic

module X =
  struct
    @type 'a t = 'a rat with show, gmap, html, eq, compare, foldl, foldr, fmt
    let fmap f x = GT.gmap (t) f x
  end

include X

module F = Fmap (X)

@type ground = LNat.ground t        with show, html, eq, compare, foldl, foldr, gmap, fmt
@type logic = LNat.logic t logic'   with show, html, eq, compare, foldl, foldr, gmap, fmt

type groundi = (ground, logic) injected

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
      method show    = GT.show(logic') (fun l -> GT.show(t) this#show l)
    end
}

let of_int_ratio = fun (a,b) -> LNat.(of_int a, of_int b)
let to_int_ratio = fun (a,b) -> LNat.(to_int a, to_int b)

let inj = fun x -> LPair.inj LNat.inj LNat.inj x

let rec reify h n = F.reify reify h n
let rec prjc onvar env n = F.prjc (prjc onvar) onvar env n

let rec rat n = Logic.inj @@ F.distrib @@ X.fmap rat n
    
let  mulo x y z =
  Fresh.(succ five) (fun nx dx ny dy nz dz->  (* n- : numerator; d- : denominator *)
      (x === LPair.pair nx dx)     &&& 
      ((y === LPair.pair ny dy)    &&&
      ((LNat.mulo nx ny nz)  &&&
      ((LNat.mulo dx dy dz)  &&&  
      (z === LPair.pair nz dz))))) (* &&& is left associative. 
                                      We force right association for speed  *)

let ( * ) = mulo

let divo x y z =
  Fresh.two (fun ny dy ->
      (y === LPair.pair ny dy) &&&
      mulo x (LPair.pair dy ny) z)

let ( / ) = divo

(*
let addo x y z =
  Fresh.

val ( + ) : groundi -> groundi -> groundi -> goal
*)

(* For the mining puzzle , these are nor needed

(** Comparisons *)
val leo : groundi -> groundi -> LBool.groundi -> goal
val geo : groundi -> groundi -> LBool.groundi -> goal
val gto : groundi -> groundi -> LBool.groundi -> goal
val lto : groundi -> groundi -> LBool.groundi -> goal

(** Comparisons as goals *)
val (<=) : groundi -> groundi -> goal
val (>=) : groundi -> groundi -> goal
val (>)  : groundi -> groundi -> goal
val (<)  : groundi -> groundi -> goal

*)




