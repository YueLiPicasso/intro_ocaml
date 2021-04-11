(* structure-mode unit implementation *)
structure Reduce :> Reduce = struct
     local open Expr in
     
     val negate = fn (Neg e) => e | e => Neg e;

     val rec reduce = fn
	 (Neg (Neg e)) => e
       | (Neg e) => negate (reduce e)
       | (Plus {1=Cst 0, 2=e2}) => reduce e2
       | (Plus {1=e1, 2=Cst 0}) => reduce e1
       | (Plus {1=e1, 2=e2}) => Plus {1=reduce e1, 2=reduce e2}
       | e => e;
     end;
 end;
