open OCanren;;
module L = List ;;
open OCanren.Std;;

(* aliasing the standard OCanren type constructor [logic] *)
@type 'a logic' = 'a logic with show, gmap;;

module BooleanTypes = struct
  @type boolean = O | I with show, gmap;;
  @type t = boolean  with show, gmap;;
  @type ground = t  with show, gmap;;
  @type logic = t logic' with show, gmap;;
  type groundi = (ground, logic) injected;;
end;;

(* constants in different sizes: constntN is an N-bit binary number *)

module Constnt2Types = struct
  @type 'boolean constnt2 = ('boolean, 'boolean) Pair.t with show, gmap;;
  @type 'b t = 'b constnt2 with show, gmap;;
  @type ground = BooleanTypes.ground t with show, gmap;;
  @type logic = BooleanTypes.logic t logic'
   with show, gmap;;
  type groundi = (ground, logic) injected;;
end;;

module Constnt3Types = struct
  @type ('b,'c2) constnt3 = ('b, 'c2) Pair.t with show, gmap;;
  @type ('b,'c2) t = ('b,'c2) constnt3 with show,gmap;;
  @type ground = (BooleanTypes.ground, Constnt2Types.ground) Pair.ground
   with show,gmap;;
  @type logic = (BooleanTypes.logic, Constnt2Types.logic) Pair.logic
   with show,gmap;;
  type groundi = (ground, logic) injected;; 
end;;

module Constnt4Types = struct
  @type ('b,'c3) constnt4 = ('b, 'c3) Pair.t with show, gmap;;
  @type ('b,'c3) t = ('b,'c3) constnt4 with show, gmap;;
  @type ground = (BooleanTypes.ground, Constnt3Types.ground) Pair.ground
   with show, gmap;;
  @type logic = (BooleanTypes.logic, Constnt3Types.logic) Pair.logic
   with show, gmap;;
  type groundi = (ground, logic) injected;;
end;;

(* use four-bit constant. Change here if wider constants are used *)
module Constant = Constnt4Types;;

(* arrays in different sizes: arrN is an N-cell array *)

module Arr2Types = struct
  @type 'constnt arr2  = ('constnt, 'constnt) Pair.t with show, gmap;;
  @type 'c t = 'c arr2 with show, gmap;;
  @type ground = Constant.ground t with show, gmap;;
  @type logic = Constant.logic t logic' with show, gmap;;
  type groundi = (ground, logic) injected;; 
end;;

module Arr4Types = struct
  @type 'arr2 arr4  = ('arr2, 'arr2) Pair.t with show, gmap;;
  @type 'a t = 'a arr4 with show, gmap;;
  @type ground = Arr2Types.ground t with show, gmap;;
  @type logic = Arr2Types.logic t logic' with show, gmap;;
  type groundi = (ground, logic) injected;;
end;;

@type 'arr4    arr8  = ('arr4, 'arr4) Pair.t with show, gmap;;
@type 'arr8    arr16 = ('arr8, 'arr8) Pair.t with show, gmap;;

module ArrayAccess = struct
  let branch :
    BooleanTypes.groundi ->
    ('a,'b,'a,'b) Pair.groundi -> ('a, 'b) injected -> goal
    = fun b ar c -> let b0 = !!(BooleanTypes.O) and b1 = !!(BooleanTypes.I) in
      ocanren{ { b == b0 & fresh c' in ar == (c, c') }
             | { b == b1 & fresh c' in ar == (c', c) }};;

  let acc_arr2 :
    BooleanTypes.groundi -> Arr2Types.groundi -> Constant.groundi -> goal
    = fun b ar c -> branch b ar c;;

  let acc_arr4 :
    Constnt2Types.groundi -> Arr4Types.groundi -> Constant.groundi -> goal
    = fun c ar c' -> 
      ocanren{ fresh b1,b2,arr2 in
        c == (b1, b2)
        & branch b1 ar arr2
        & acc_arr2 b2 arr2 c' };;

end;;

(* a variable is a character string *)

@type ('c,'v,'self) expr = Con of 'c
                         | Var of 'v
                         | Arr of 'v * 'self
                         | Brh of 'self * 'self * 'self
 with show, gmap;;


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
