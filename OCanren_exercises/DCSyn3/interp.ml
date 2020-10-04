(** naive interpreters of the two langauges, designed with forward execution in mind  *)
open OCanren;;
open OCanren.Std;;
open Coar;;
open Syntax;;

let rec eval_imp : State.groundi -> Expr.groundi -> Value.groundi -> goal
    = fun s e v ->
      ocanren {
        {fresh c in e == Con c & v == Conv c }
      | {fresh va in e == Var va & List.assoco va s v}
      | {fresh va, ex, ar, idx, c in
         e == Arr (va, ex)
         & List.assoco va s (Arrv ar)
         & eval_imp s ex (Conv idx)    
         & ArrayAccess.rel idx ar c
         & v == Conv c}
      | {fresh e1,e2,e3,v' in
         e == Brh (e1,e2,e3)
         & eval_imp s e1 v'
         & {v' == Conv Constant.zero & eval_imp s e3 v
          | v'=/= Conv Constant.zero & eval_imp s e2 v}}};;

let rec eval_sig : State.groundi -> Signal.groundi -> Value.groundi -> goal
    = fun s e v ->
      ocanren {
      {fresh c in e == Src c & v == Conv c }
    | {fresh va in e == Port va & List.assoco va s v}
    | {fresh e1,e2,e3,v' in
       e == Mux (e1,e2,e3)
       & eval_sig s e1 v'
       & { v' == Conv Constant.zero & eval_sig s e3 v
         | v'=/= Conv Constant.zero & eval_sig s e2 v }}
    | {fresh e1, e2, c, ar, idx, ar',idx', ar'',idx'' in
       e == Slice (e1, e2)
       & eval_sig s e1 (Arrv ar)
       & eval_sig s e2 (Conv idx)    
       & ArrayAccess.rel idx ar c
       & v == Conv c}
    | {fresh va,e1,e2,ve1,s' in
       e == Fout (va, e1, e2)
       & eval_sig s e1 ve1
       & s' == (va, ve1) :: s
       & eval_sig s' e2 v}};;


module NoLet = struct
  let rec eval_sig : State.groundi -> Signal.groundi -> Value.groundi -> goal
      = fun s e v ->
      ocanren {
      {fresh c in e == Src c & v == Conv c }
    | {fresh va in e == Port va & List.assoco va s v}
    | {fresh e1,e2,e3,v' in
       e == Mux (e1,e2,e3)
       & eval_sig s e1 v'
       & { v' == Conv Constant.zero & eval_sig s e3 v
         | v'=/= Conv Constant.zero & eval_sig s e2 v }}
    | {fresh e1, e2, c, ar, idx, ar',idx', ar'',idx'' in
       e == Slice (e1, e2)
       & eval_sig s e1 (Arrv ar)
       & eval_sig s e2 (Conv idx)    
       & ArrayAccess.rel idx ar c
       & v == Conv c}};;

  let rec syn : Specs.groundi -> Signal.groundi -> goal = fun ss p -> ocanren {
        ss == []
      | fresh st,va,ss' in ss == (st, va) :: ss' & eval_sig st p va & syn ss' p };; 
end;;
