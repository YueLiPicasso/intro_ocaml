open Logic
open Core
open LPair
open LNat
    

(** the type for ground rational numbers *)
@type 'a t = 'a * 'a with show, html, eq, compare, foldl, foldr, gmap, fmt

(** Type synonym to prevent toplevel [logic] from being hidden *)
@type 'a logic' = 'a logic with show, html, eq, compare, foldl, foldr, gmap, fmt

let logic' = logic

module X =
  struct
    @type 'a t = 'a nat with show, gmap, html, eq, compare, foldl, foldr, fmt
    let fmap f x = GT.gmap (t) f x
  end

include X

module F = Fmap (X)

(** Logic rational *)
@type logic = Nat.logic t logic' with show, html, eq, compare, foldl, foldr, gmap, fmt

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

(** Ground rationals are ismorphic for regular one *)
@type ground = Nat.ground t with show, html, eq, compare, foldl, foldr, gmap, fmt


(** [of_int n] converts integer pair [(m,n)] into [ground]; negative integers become [O] *)
let  of_int : int * int -> ground =
  fun (a,b) -> (of_int a, of_int b)

(** [to_int g] converts ground [g] into integer pair *)
let to_int : ground -> int * int =
  fun (a,b) -> (to_int a, to_int b)

(** Logic injection (for reification) *)
let inj : ground -> logic =  
   LPair.inj LNat.inj LNat.inj 

(** A type synonym for injected rational *)
type groundi = (ground, logic) injected

let rec reify h n = F.reify reify h n

let rec prjc onvar env n = F.prjc (prjc onvar) onvar env n

(** Make injected [rational] from ground one *)
let rec rat n = Logic.inj @@ F.distrib @@ X.fmap rat n
    

(* After the reification stage: free variables make these impossible 

(** the greatest common divisor *)

(** the least common multiple *)

*)
    
(** Relational addition *)
val addo  : groundi -> groundi -> groundi -> goal

(** Infix syninym for [addo] *)
val ( + ) : groundi -> groundi -> groundi -> goal

(** Relational multiplication *)
let  mulo x y z =
  Fresh.(succ five) (fun nx dx ny dy nz dz->  (* n- : numerator; d- : denominator *)
      (x === pair nx dx)     &&& 
      ((y === pair ny dy)    &&&
      ((LNat.mulo nx ny nz)  &&&
      ((LNat.mulo dx dy dz)  &&&  
      (z === pair nz dz)))))

(* left associative &&&; but we want right association, which is faster  *)

(** Infix syninym for [mulo] *)
let ( * ) = mulo

(** Relational division *)
let divo x y z =
  Fresh.two (fun ny dy ->
      (y === pair ny dy) &&&
      mulo x (pair dy ny) z)

(** Infix syninym for [divo] *)
let ( / ) = divo

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




