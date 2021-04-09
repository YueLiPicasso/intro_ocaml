(* structure-mode unit implementation *)

structure Expr = struct

  datatype expr = Cst of int | Neg of expr | Plus of expr * expr
							      
  val rec show = fn v => (fn 
     ((Cst n))            => makestring n
   | ((Neg e))            => "(-" ^ show e ^ ")"
   | ((Plus {1=e1,2=e2})) =>  "(" ^ show e1 ^ "+" ^ show e2 ^ ")" ) ((v))
								    
end;
		 
(* Do some syntactic transformation to convert from Core to Bare.

  Start with :

  fun show (Cst n)        = makestring n
    | show (Neg e)        = "(-" ^ show e ^ ")"
    | show (Plus (e1,e2)) = "(" ^ show e1 ^ "+" ^ show e2 ^ ")"

  We highlight the fvalbind part : 

      show (Cst n)        = makestring n
    | show (Neg e)        = "(-" ^ show e ^ ")"
    | show (Plus (e1,e2)) = "(" ^ show e1 ^ "+" ^ show e2 ^ ")" 
  
  Apply rewriting rule to fun, we get 

  val rec fvalbind

  which is an intermediate form but not part of the Core syntax.  
  
  Apply rewriting rule to fvalbind, we get (and now we are back in the Core syntax): 

  val rec show = fn v => case (v) of
     ((Cst n))        => makestring n
   | ((Neg e))        => "(-" ^ show e ^ ")"
   | ((Plus (e1,e2))) => "(" ^ show e1 ^ "+" ^ show e2 ^ ")"

  Apply rewriting rule to case..of, we get:

  val rec show = fn v => (fn 
     ((Cst n))        => makestring n
   | ((Neg e))        => "(-" ^ show e ^ ")"
   | ((Plus (e1,e2))) =>  "(" ^ show e1 ^ "+" ^ show e2 ^ ")" ) ((v)) 

  Rewrite the last pattern:
 
  val rec show = fn v => (fn 
     ((Cst n))            => makestring n
   | ((Neg e))            => "(-" ^ show e ^ ")"
   | ((Plus {1=e1,2=e2})) =>  "(" ^ show e1 ^ "+" ^ show e2 ^ ")" ) ((v)) 

  The above dec is in the Bare syntax.
*)				

							      
