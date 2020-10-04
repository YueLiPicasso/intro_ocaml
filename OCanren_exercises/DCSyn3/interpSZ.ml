(** naive interpreters of the two langauges,
    designed with forward execution in mind, with size bound  *)
open OCanren;;
open OCanren.Std;;
open Coar;;
open Syntax;;

(** Given state [s], the program [e] evaluates to [v], where [e] has [n] constructors *)
let rec eval_imp : State.groundi -> Expr.groundi -> Value.groundi -> Nat.groundi-> goal
    = fun s e v n ->
      ocanren {
        {fresh c in e == Con c & v == Conv c & n == Nat.one}
      | {fresh va in e == Var va & List.assoco va s v & n == Nat.one}
      | {fresh va, ex, ar, idx, c, n' in
         e == Arr (va, ex)
         & v == Conv c
         & n == Nat.S n'
         & List.assoco va s (Arrv ar)
         & eval_imp s ex (Conv idx) n'   
         & ArrayAccess.rel idx ar c
          }
      | {fresh e1,e2,e3,v1,v2,v3,n1,n2,n3,n',n'' in
         e == Brh (e1,e2,e3)
         & {v1 == Conv Constant.zero & v == v3
           | v1 =/= Conv Constant.zero & v == v2}
         & n == Nat.S n''
         & eval_imp s e1 v1 n1
         & eval_imp s e2 v2 n2
         & eval_imp s e3 v3 n3
         & Nat.addo n1 n2 n'
         & Nat.addo n3 n' n''}};;

module NoLet = struct
  let rec eval_sig : State.groundi -> Signal.groundi -> Value.groundi -> Nat.groundi -> goal
      = fun s e v n ->
      ocanren {
      {fresh c in e == Src c & v == Conv c & n == Nat.one}
    | {fresh va in e == Port va & List.assoco va s v & n == Nat.one}
    | {fresh e1,e2,e3,v1,v2,v3,n1,n2,n3,n',n'' in
       e == Mux (e1,e2,e3)
       & { v1 == Conv Constant.zero & v == v3
         | v1 =/= Conv Constant.zero & v == v2 }
       & n == Nat.S n''
       & eval_sig s e1 v1 n1
       & eval_sig s e2 v2 n2
       & eval_sig s e3 v3 n3
       & Nat.addo n3 n' n''
       & Nat.addo n1 n2 n'}
    | {fresh e1, e2, c, ar, idx, ar',idx', ar'',idx'',n1,n2,n' in
       e == Slice (e1, e2)
       & v == Conv c
       & n == Nat.S n'
       & eval_sig s e1 (Arrv ar) n1
       & eval_sig s e2 (Conv idx) n2
       & Nat.addo n1 n2 n'
       & ArrayAccess.rel idx ar c}};;

  let rec syn : Specs.groundi -> Signal.groundi -> Nat.groundi -> goal =
      fun ss p n -> ocanren {
        ss == []
      | fresh st,va,ss' in ss == (st, va) :: ss' & eval_sig st p va n & syn ss' p n};; 
end;;





