structure ARITH =
 struct
   datatype NAT = Zero | Succ of NAT
   fun twice (Zero) = Zero
     | twice (Succ x) = Succ (Succ (twice x))
 end;

open ARITH;

val two = Succ (Succ Zero);

twice two;


