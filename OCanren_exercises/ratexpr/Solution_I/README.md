# Solution I

A half-successful solution to the problem. Sections on problems below are in
anti-chronological order.

## Highlight

The types have been gotten right. The Eulidean Algorithm was implemented.


## Problem and the cause thereof

Now I have two variants of the relation `eval`, one is optimized for forward
execution amd cannot be used for backward execution, and the other is for backward execution
and cannot be used for forward execution.

### The forward evaluator

```ocaml
 (** for forward use *)
  let rec eval ex no =
    let open Inj in let open LNat in
    conde [
      Fresh.four (fun a b a' b' ->
          ?& [ex === num a b ; no === num a' b' ; simplify a b a' b']);
      Fresh.two (fun ea eb ->
          ?& [ex === sum ea eb ;
              Fresh.two (fun na nb ->
                  ?& [eval ea na ; eval eb nb;
                      Fresh.four (fun a b a' b' ->
                          ?& [na === num a b ; nb === num a' b' ;
                              Fresh.four (fun ab' a'b bb' nu ->
                                  ?& [( * ) a b' ab';
                                      ( * ) a' b a'b;
                                      ( * ) b b' bb';
                                      ( + ) ab' a'b nu;
                                      Fresh.two (fun nu' bb'' ->
                                          ?& [simplify nu bb' nu' bb'';
                                              no === num nu' bb''])])])])]);
      Fresh.two (fun ea eb ->
          ?& [ex === subt ea eb ;
              Fresh.two (fun na nb ->
                  ?& [eval ea na ; eval eb nb;
                      Fresh.four (fun a b a' b' ->
                          ?& [na === num a b ; nb === num a' b' ;
                              Fresh.four (fun ab' a'b bb' nu ->
                                  ?& [( * ) a b' ab';
                                      ( * ) a' b a'b;
                                      ( * ) b b' bb';
                                      ( + ) nu a'b ab' ;
                                      Fresh.two (fun nu' bb'' ->
                                          ?& [simplify nu bb' nu' bb'';
                                              no === num nu' bb''])])])])]);
      Fresh.two (fun ea eb ->
          ?& [ex === sum ea eb ;
              Fresh.two (fun na nb ->
                  ?& [eval ea na ; eval eb nb;
                      Fresh.four (fun a b a' b' ->
                          ?& [na === num a b ; nb === num a' b' ;
                              Fresh.four (fun ab a'b' s1 s2 ->
                                  ?& [( * ) a b ab;
                                      ( * ) a' b' a'b';
                                      simplify ab a'b' s1 s2;
                                      no === num s1 s2])])])])];;
```


### The backward evaluator

```ocaml
(** for backward use *)
  let rec eval' ex no =
    let open Inj in let open LNat in let open LPair in 
    conde [
      Fresh.two (fun a b -> (*not using  simplify' here *)
          ?& [ex === num a b ; no === num a b ]);
      Fresh.(succ five) (fun ea eb nu1 de1 nu2 de2  ->
          ?& [ex === sum ea eb ;
              no === num  nu1 de1;
              simplify' nu2 de2 nu1 de1;
              Fresh.(succ five) (fun a a' sa sa' sde2 sde2' ->
                  ?& [( + ) a   a'  nu2 ;
                      simplify a  de2 sa  sde2  ;
                      simplify a' de2 sa' sde2' ;
                      Fresh.two (fun na nb ->
                          ?& [na === num sa  sde2 ;
                              nb === num sa' sde2';
                              eval' ea na ;
                              eval' eb nb ])])]);
    Fresh.(succ five) (fun ea eb nu1 de1 nu2 de2  ->
          ?& [ex === subt ea eb ;
              no === num  nu1 de1;
              simplify' nu2 de2 nu1 de1;
              Fresh.(succ five) (fun a a' sa sa' sde2 sde2' ->
                  ?& [( + ) a'  nu2  a;
                      simplify a  de2 sa  sde2  ;
                      simplify a' de2 sa' sde2' ;
                      Fresh.two (fun na nb ->
                          ?& [na === num sa  sde2 ;
                              nb === num sa' sde2';
                              eval' ea na ;
                              eval' eb nb ])])]);
    Fresh.(succ five) (fun ea eb nu1 de1 nu2 de2  ->
          ?& [ex === prod ea eb ;
              no === num  nu1 de1;
              simplify' nu2 de2 nu1 de1;
              Fresh.(succ @@ succ @@ succ @@ five) (fun a a' b b' sa sa' sb sb' ->
                  ?& [( * ) a   a'  nu2 ;
                      ( * ) b   b'  de2 ;
                      simplify a  b sa  sb  ;
                      simplify a' b' sa' sb' ;
                      Fresh.two (fun na nb ->
                          ?& [na === num sa  sb ;
                              nb === num sa' sb';
                              eval' ea na ;
                              eval' eb nb ])])]);];;
```

## Problem and the cause thereof 

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
