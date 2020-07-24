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

let num  (x, y) = inj @@ F.distrib (Num  (x, y))
and sum  (x, y) = inj @@ F.distrib (Sum  (x, y))
and subt (x, y) = inj @@ F.distrib (Subt (x, y))
and prod (x, y) = inj @@ F.distrib (Prod (x, y));;

