# Solution I

A half-successful solution to the problem.

## Highlight

Relational implementation of Eulidean Algorithm followed by relational
simplification of rational numbers.

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
run q (fun q -> ocanren { eval q (Num (1,3)) } )) (* etc. *)
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
at the top-level does not has the `*` type constructor at all, and which is
therefore clashes with my definition of the abstract `rat_expr` type. It shall
be modified to something like:

```ocaml
@type ('a,'b) rat_expr =
   | Num of 'a
   | Sum of 'b
   | Prod of 'b
   | Subt of 'b
```

where `'a` is supposed to be instantiated by _logical pairs_ of nats at
_ground_, _logic_ and _injected_ level, and `'b` is supposed to be
instantiated by logical pairs of rat_expr (recursively) at those levels
as well.   


