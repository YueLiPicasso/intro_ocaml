# Solution I

The discussion below is not about the most recent version, but about various intermediate
versions, on their good characters or problems 

## Relational Simplification of Rational Numbers

We can use a single algorithm named `simplify''` ,  where  `simplify'' a b a' b'` is true
iff the rational number `a/b` has  normal form `a'/b'`, to

- (_Forward_) simplify a rational number to the normal form
- (_Backward_) scale up a normal form rational number 
- (_Quine_) find rational numbers whose numerator and denominator are co-prime. 

Below we see how this is done. 


I first came up with two versions, named  `simplify` and `simplify'`, which are good at
forward and backward
execution resp. but not the other way round. Further observation of the internals of these
definitions helped with the definition of a third version `simplify''`, which is satisfactorily
relational. We first compare `simplify` with `simplify''`.  

```ocaml

let simplify a b a' b'=
    let open LNat in let open LoNat in
    conde [
      (?& [a === b ; a' === one ; b' === one]);
      (?& [b < a ; Fresh.one (fun q -> (?& [gcd a b q ; ( * ) q a' a ; ( * ) q b' b]))]);
      (?& [a < b ; Fresh.one (fun q -> (?& [gcd b a q ; ( * ) q a' a ; ( * ) q b' b]))])];;
      

let simplify'' a b a' b'=
    let open LNat in let open LoNat in
    conde [
      (?& [a === b ; a' === one ; b' === one]);
      (?& [b < a ; Fresh.one (fun q -> (?& [( * ) q b' b ; ( * ) q a' a ; gcd a b q ]))]);
      (?& [a < b ; Fresh.one (fun q -> (?& [( * ) q a' a ; ( * ) q b' b ; gcd b a q ]))])];;
``` 

The difference is just at the order of conjuncts. Before we proceed, note that when both `a` and `b` are fresh, `b < a` generates concrete values for `b` only,
and answers for `a` are not concrete, like:

`b`   | `a`
--- | ---
O   |  \_.1 [\_.1 =/= O]
S O |  S \_.1 [\_.1 =/= O]
S (S O) | S (S \_.1) [\_.1 =/= O]
S (S (S O)) | S (S (S \_.1)) [\_.1 =/= O]
etc. | etc.

Now we continue. Without loss of generality, _we focus on the second clause of each definition:_

```ocaml
(* simplify   *)
(?& [b < a ; Fresh.one (fun q -> (?& [gcd a b q ; ( * ) q a' a ; ( * ) q b' b]))])

(* simplify'' *)
(?& [b < a ; Fresh.one (fun q -> (?& [( * ) q b' b ; ( * ) q a' a ; gcd a b q ]))])
``` 

 The table below summarizes the comparison.

 
 Semantics |      `simplify`      |      `simplify''`
 ---  | ---  | ---
 Forward  | Straightforward: find the gcd, then compute `a'` and `b'` by dividing with `q`.| Factor `b` into `q` and `b'`, then try to factor `a` using `q`. If it succeeds, then we get `a'` and we know that `q` is a common divisor of `a` and `b`; if it fails,  refactor `b` until `q` is a common divisor. Finally  test that `q` is the gcd, and if not, refactor `b` until `q` is the gcd, and at this moment `a'/b'` is just the normal form.  The total cost is bounded by the size of `b`. 
 Backward | Generate `a`, `b` and  `q` such that `q` is the gcd of `a` and `b`, then test if divding `a/b` by `q` happens to be `a'/b'`. Very inefficient for it has to traverse the enumerable space of rational numbers and there is always some number that comes late when you fix a traversal strategy. In our case `gcd a b q` traverses the space of `(a,b,q)` by increasing `a` much faster than `b`. The implication is that finding large  multiples of `a'/b'` is inproportionately time consuming. | Straightforward: generate `q` and `b` such that `q * b' = b`. Then compute `a` by `a' * q = a`. Then test `gcd a b q`  which must be true for  `a'` and `b'` are coprime which is  assumed.


For the sake of completeness of discussion, `simplify'` is defined as

```ocaml
 let simplify' a b a' b' = let open LNat in let bnd = nat @@ of_int 10 in 
    Fresh.one (fun k -> ?& [ k <= bnd ; k =/= zero ; ( * ) a' k a ; ( * ) b' k b] );;
```

It efficiently computes multiples of `a'/b'` but when used forward it does not always generate
the normal form: it actually enumerates all scaled-down numbers before reaching the normal form.

#### The philosophy is about compromise 

We conclude that  non-commutativity of conjuncts breaks down to
the role played by each conjunct in the generate-and-test process. We need to decide which
conjunct shall be used to generate and which to test, such that the conjunction as  a whole 
best accommodates different directions of relation
execution. In our case using `( * )` to generate and `gcd` to test allows productive  compromise
between forward and backward efficiency, whilst using `gcd` to generate and `( * )` to test
maximizes forward efficiency at the cost of substantially losing backward efficiency. Similarly, `simplify'` omits the `gcd` step thus speeds up the backward
direction at the cost of losig forward correctness. The philosophy is about
compromise: if you want bidrectional support, you have to accpet that the
performance on each direction is not as good as the design that is optimized
for that direction, and if you want excellent performance on one direction then you have to accept that it does not support bidirectional execution !  



# Problem Tracking

Discussions below are in
anti-chronological order: older issues are closer to the bottom of the page.


## Problem and the cause thereof

Now we can have a relation (named `eval''`) that achieves
all three goals of this project. But there is still one more thing, which is  bounding
the numerator and denominator by a small interger (say, 100),  worth be to tried
for more interesting answers from the queries.  

There are also two variants (resp. `eval` and `eval'`)
which is optimized resp. for forward run  and backward run.

For `eval''`  the compromise is that it
 can run in both directions, but
 for each direction it is not as efficient as the variant that is optimized for that
 direction.


A simple disjunction
(named `eval'''`) of `eval` and `eval'` also allows execution in all directions:
when used forward or to find quines, it behaves like `eval` and when used backward it
behaves like `eval'`. These all are based on  the _interleaved search_ of OCanren: it gets the
best of each disjunct and makes no compromise. 



## Problem and the cause thereof

Based on the relational simplifier `simplify''`, I got a
half-successful relational evaluator `eval''` for
rational number arithmetic expressions.

```ocaml
let rec eval'' ex no =
    let open Inj in  let open LNat in
    ocanren {
      {fresh a, b, a', b' in                              (* first clause *)
         ex == Num (a, b)
       & no == Num (a', b')
       & simplify'' a b a' b' }
     | 
      {fresh ea, eb, na, nb, a, b, a', b',                (* second clause *)
             ab', a'b, nu, bb', nu', bb'' in
         ex == Sum (ea, eb)       
       & eval'' ea na              
       & eval'' eb nb              
       & na == Num (a, b)          
       & nb == Num (a', b')        
       & ( * ) a   b'  ab'         
       & ( * ) a'  b   a'b         
       & ( * ) b   b'  bb'         
       & ( + ) ab' a'b nu          
       & simplify'' nu bb' nu' bb'' 
       & no == Num (nu', bb'') }
     |
      {fresh ea, eb, na, nb, a, b, a', b',                (* third clause *)
             ab', a'b, nu, bb', nu', bb'' in
         ex == Subt (ea, eb)       
       & eval'' ea na              
       & eval'' eb nb              
       & na == Num (a, b)          
       & nb == Num (a', b')        
       & ( * ) a   b'  ab'         
       & ( * ) a'  b   a'b         
       & ( * ) b   b'  bb'         
       & ( + ) nu  a'b ab'          
       & simplify'' nu bb' nu' bb'' 
       & no == Num (nu', bb'') }
     |
      {fresh ea, eb, na, nb, a, b, a', b',                 (* fourth clause *)
             aa', bb', s1, s2 in
         ex == Prod (ea, eb)
       & eval'' ea na
       & eval'' eb nb
       & na == Num (a, b)
       & nb == Num (a', b')
       & ( * ) a  a' aa'
       & ( * ) b  b' bb'
       & simplify'' aa' bb' s1 s2
       & no == Num (s1, s2) } };;

```

When asking what expression evaluates to a given number, only the first clause in the
evaluator contributes answers, finding multiples of that number, but all the rest clauses
involving addition, subtruction and multiplication rarely contribute answers. This is
due to the inefficient backward searching algorithm. In those clauses it
generates two equations and checks if the two right-hand-sides happen to sum / subtruct / product
to the given number. The search space is too large. 

## Problem and the cause thereof

Now I have two variants of the relation `eval`, one is optimized for forward
execution and cannot be used for backward execution, and the other is for backward execution
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

In the first clause,
```ocaml
Fresh.four (fun a b a' b' ->
          ?& [ex === num a b ; no === num a' b' ; simplify a b a' b']);
```
the relation `simplify` is not efficient when being used backward: it is based
on an implementation of the Euclidean
algorithm and  is good at simplifying but not complicating rational numbers.
```ocaml
let simplify a b a' b'=
    let open LNat in let open LoNat in
    conde [
      (?& [a === b ; a' === one ; b' === one]);
      (?& [b < a ; Fresh.one (fun q -> (?& [gcd a b q ; ( * ) q a' a ; ( * ) q b' b]))]);
      (?& [a < b ; Fresh.one (fun q -> (?& [gcd b a q ; ( * ) q a' a ; ( * ) q b' b]))])];;
```

The source code
contains a detailed record of an experiment on `simplify`, copied below.

```ocaml
(** find  numbers [q], [r] and [s] such that [gcd q r s] for some r < q. This mimics
    the internals of [simplify] when it is used backward. We could see that [q] grows
    much faster than [r]. We also guess the way [simplify] works backward is that
    it generates pairs of numbers together with their gcd, and checks if they simplify to
    the given number. This two combined, we say that when simplify is used backward to 
    find a small number of answers, it could work fast. But when  it is asked to find a large
    number of answers, due to the fact that [gcd] does not produce evenly distributed 
    answers, this would prolong the waiting time indefinitely. We could further sort the
    answers to see the relative speed of growth of [q] and [r].  We could see that in 
    the 1000 answers, when [r] stayed at 1, [q] ranged from 2 to 240, and when [r] stayed 
    at 2, [q] ranged from 3 to 240; similar for [r] equals 3. When [r] stayed at 4, [q] grown
    to 188 from 5; when [r] is 5, [q] grown from 6 to 160; [r] 6, [q] 7 to 138; 
    [r] 7, [q] 8 to 112; [r] 8, [q] 9 to 72; the biggest [r] is 14 before the process was 
    killed by the system automatically. *)
let _ =
  let compr = fun (a,b,c) (a',b',c') -> match compare b b' with
    | 0 -> compare a a'
    | c -> c
  and  li = RStream.take ~n:1000 @@
    run qrs (fun q r s -> LNat.( < ) r q &&& LoNat.gcd q r s )
      (fun q r s-> LNat.to_int @@ project q,
                   LNat.to_int @@ project r,
                   LNat.to_int @@ project s)
  in  List.iter (fun x -> print_string @@ GT.show(pr3) x ;  print_newline())
  @@ List.fast_sort compr li
    
 ;;

```
Note the execution of the `test.opt` file, when asking for 1000 answers from the query above: sometimes it gave all the answers,
but some other times it was kiled without any answer. The advisable way to run this file is to close all other applications and don't
even touch the mouse until it finishes.

It worthes to try a simplfied version of `gcd`, from
```ocaml
let rec gcd a b c =
    conde [(?& [b <= a ; divisible_by a b ; c === b]);
           (?& [b < a ; Fresh.one (fun r -> (?& [remainder a b r; r =/= zero; gcd b r c]))])];;

```
to
```ocaml
let rec gcd a b c =
    conde [(?& [divisible_by a b ; c === b]);
           (?& [Fresh.one (fun r -> (?& [remainder a b r; r =/= zero; gcd b r c]))])];;

```
where we implicitly require that `a <= b`. This removes an extra layer of backtraking on
comparison, which has already been done by `simpify` before it calls `gcd`. _Follow up
experiemnts shown that this does not make any difference at all in the above query. A more
plausible way was found, which is to swap the conjuncts in `simplify` so that when being
used backward it generates multiples and trivially passes the gcd and when being used forward
it has a bit more overhead of traversing all common divisors._ 



In the second clause,
```ocaml
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
```
when the program is used backward, both calls
```ocaml
eval ea na ; eval eb nb;
```
will be made with all arguments being fresh variables. This is is also a source of non-termination.
 Similar for the third and fourth clause.
 
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
