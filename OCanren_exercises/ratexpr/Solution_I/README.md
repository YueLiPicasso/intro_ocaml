# Solution I

A half-successful solution to the problem.

## Highlight

The types have been gotten right. The Eulidean Algorithm was implemented.

## Problem and the cause thereof

When using `LoRat.eval` to generate arithmetic expressions that evaluate to
a specified normalized rational number, substitution was not performed
properly, so that some logic variables are not properly instantiated,
causing results like:

```ocaml
_.10
_.10
Num (S (O), S (S (S (O))))
Num (S (S (O)), S (S (S (S (S (S (O)))))))
Num (S (S (S (O))), S (S (S (S (S (S (S (S (S (O))))))))))
Num (S (S (S (S (O)))), S (S (S (S (S (S (S (S (S (S (S (S (O)))))))))))))
Sum (_.15, _.16)
Num (S (S (S (S (S (O))))), S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (O))))))))))))))))
Sum (_.15, _.16)
Sum (_.15, _.16)
Sum (_.15, _.16)
```
when I asked the program to  find expressions that reduces to `1/3`.

### Cause

Perhaps because I did not use `LPair` at all, the library for logical pairs.
This made my programming style at a later point diverge from the familiar way.
For instance, the query is usually written as :

```ocaml
run q (fun q -> ocanren { eval q (Num (1,3)) } ) (* etc. *)
```
 but in my setup I had to  write like:
 
```ocaml
run q (fun q -> eval q (num (ocanren{1}, ocanren{3})))  (* etc. *)
```

The `ocanren {}` structure executes a syntactic transformation that converts
individual digits 1 and 3 to injected natural numbers, and in turn converts
`(1,3)` to a logical pair. It also converts the constructor `Num` to lower
case `num` which is a user-defined injection primitive. In my code the
injection primitive `num` accepts a normal pair of injected nats but in the
expected style it shall accept an injected pair of type LPair.groundi, which
at the top-level does not has the `*` type constructor at all:

```ocaml
(** expected *)
val num :
  (LNat.ground, LNat.logic, LNat.ground, LNat.logic) LPair.groundi -> Rat_expr.groundi;;
(** mine, problematic *)
val num : LNat.groundi * LNat.groundi -> Rat_expr.groundi
```

To resolve this clash, I must further modify the definition of the
abstract `rat_expr` type. It shall now be like:

```ocaml
@type ('a,'b) rat_expr =
   | Num of 'a
   | Sum of 'b
   | Prod of 'b
   | Subt of 'b
 with (* etc. *)
```

where `'a` is supposed to be instantiated by _logical pairs_ of nats at
_ground_, _logic_ and _injected_ level, and `'b` is supposed to be
instantiated by logical pairs of rat_expr (recursively) at those levels
as well.   

## Problem and the cause thereof (follow up)

In a discussion with Dmitri Boulytchov, the original abstract type definition

```ocaml
@type ('nat, 'self) rat_expr =
     Num of 'nat * 'nat             
   | Sum of 'self * 'self           
   | Subt of 'self * 'self          
   | Prod of 'self * 'self          
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;
```

was considered as being more intuitive. He also pointed out that the
way the injection primitives were defined conld be improved. Preiously:

```ocaml
module Inj : sig
  val num  : LNat.groundi * LNat.groundi -> groundi;;
  val sum  : groundi * groundi -> groundi;;
  val subt : groundi * groundi -> groundi;;
  val prod : groundi * groundi -> groundi;;
end = struct
  let num  (x, y) = inj @@ F.distrib (Num  (x, y))
  and sum  (x, y) = inj @@ F.distrib (Sum  (x, y))
  and subt (x, y) = inj @@ F.distrib (Subt (x, y))
  and prod (x, y) = inj @@ F.distrib (Prod (x, y));;
end;;
```

He advised to change it to :

```ocaml
module Inj : sig
  val num  : LNat.groundi -> LNat.groundi -> groundi;; 
  val sum  : groundi -> groundi -> groundi;;
  val subt : groundi -> groundi -> groundi;;
  val prod : groundi -> groundi -> groundi;;
end = struct
  let num  x y = inj @@ F.distrib (Num  (x, y)) 
  and sum  x y = inj @@ F.distrib (Sum  (x, y))
  and subt x y = inj @@ F.distrib (Subt (x, y))
  and prod x y = inj @@ F.distrib (Prod (x, y));;
end;;
```

I already understood that these primitives are supposed to convert
__from__ constructor application to injected arguments, __to__ a value of the injected type
to which the constructor itself belongs.

The new definition resolves the problem with the `ocanren {}` construct. Now queries should be
made like:

```ocaml
 run q (fun q -> ocanren {eval (Sum (Num (1, 3), Num (4, 5))) q}) project
```

The `ocanren {}` construct is designed to allow writing constructor applications
in the conventional way:
```
constr (v1, ... , vn) 
```

and this is transformed to
```
constr' v1 ... vn
```
where `constr'` is lower-cased `constr` and is applied to a space-separated list of argument,
corresponding to the shape of the user-defined injection primitive.

The next thing to do is to test the relational behaviour of various components and try different
versions of a relation. 
