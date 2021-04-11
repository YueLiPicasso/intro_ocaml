(* structure-mode unit interface *)

(* keyword *)
signature

(* sigbind ::= sigid = sigexp *)
Reduce =
sig
    val reduce : Expr.expr -> Expr.expr 
end;
