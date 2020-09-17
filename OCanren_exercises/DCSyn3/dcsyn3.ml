open OCanren;;
module L = List ;;
open OCanren.Std;;

@type boolean = O | I with show;;

(* constants in different sizes: constntN is an N-bit binary number *)
@type 'boolean constnt2 = ('boolean, 'boolean) Pair.t with show, gmap;;
@type ('b,'c2) constnt3 = ('b, 'c2) Pair.t with show, gmap;;
@type ('b,'c3) constnt4 = ('b, 'c3) Pair.t with show, gmap;;    


(* a variable is a character string *)

@type ('c,'v,'self) expr = Con of 'c
                         | Var of 'v
                         | Arr of 'v * 'self
                         | Brh of 'self * 'self * 'self
 with show, gmap;;

(* arrays in different sizes: arrN is an N-cell array *)
@type 'constnt arr2  = ('constnt, 'constnt) Pait.t with show, gmap;;
@type 'arr2    arr4  = ('arr2, 'arr2) Pait.t with show, gmap;;
@type 'arr4    arr8  = ('arr4, 'arr4) Pait.t with show, gmap;;
@type 'arr4    arr16 = ('arr8, 'arr8) Pait.t with show, gmap;;


@type ('c, 'a) value = Conv of 'c | Arrv of 'a  | Undef
 with show, gmap;;

module Expr = struct
  @type ('a,'b,'c) t = ('a,'b,'c) expr with show, gmap;;
  let fmap = fun f1 f2 f3 x -> GT.gmap(t) f1 f2 f3 x;;
end;;

module Value = struct
  @type ('a,'b) t = ('a,'b) value with show, gmap;;
  let fmap = fun f1 f2 x -> GT.gmap(t) f1 f2 x;;
end;;

module Inj = struct
  module FExpr = Fmap3(Expr);;
  module FValue = Fmap2(Value);;

  let con = fun x -> inj @@ FExpr.distrib (Con x);;
  let var = fun x -> inj @@ FExpr.distrib (Var x);;
  let arr = fun x y -> inj @@ FExpr.distrib (Arr (x,y));;
  let brh = fun x y z -> inj @@ FExpr.distrib (Brh (x,y,z));;

  let conv = fun x -> inj @@ FValue.distrib (Conv x);;
  let arrv = fun x -> inj @@ FValue.distrib (Arrv x);;
  let undef = fun () -> inj @@ FValue.distrib Undef;;
  
end;;



(*


(* args: state, expr, value *)
let rec eval_imp s e v =
  ocanren {
    { fresh c in e == Con c & v == Conv c }
| {fresh va, r in e == Var va & List.assoc va s r & v == r}
| {fresh va, ex, idx in e == Arr (va, ex) & eval_imp s ex idx & v ==  }
  };;

*)
