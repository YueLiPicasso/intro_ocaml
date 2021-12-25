# How Types Are Respected? - A Case Study

A logical list of integer options cannot have the form `[Some v;v]` for any loggic variable `v` because `v` cannot at the same time be an integer (as the argument of `Some`) and an integer option (as the member of the list). The code below, by which we create the invalid list (lines 3,4,5) then reify it using the reifier (line 2) for the intended type, causes an error during machine type-checking.

```ocaml
1| let _ = print_string @@
2|   let tm = Reifier.apply (List.reify (Option.reify Reifier.reify))
3|       (run (fun v -> Env.return
4|                (inj List.(Cons(inj (Some v),
5|                               inj (Cons(v, (inj Nil)))))))) in
6|   match (tm : int Core.logic Option.logic List.logic) with
7|  | Value(Cons(Value(Some(Var _)), Value (Cons (Var _, Value Nil)))) -> "PASSED\n"
8|  | _ -> "failed\n"
```

On the one hand, it is good to have a type error here because it gives confidence over type-safty of monadic reifiers. On the other hand, the error message is _not_ quickly comprehensible, inspiring the question "How is this invalid list rejected by the type checker?". To answer this question is to understand the error message.

It turned out that a thorough (and manual) type analysis not only converges at the error message returned by the type-checker, but also inspires further discussion on several interesting aspects of the code:


- The interplay between the reifier's type and the type of the term to be reified.
- The consequence of using a reifier that is not deep enough.
- The role played by the explicit type annotation, and what if we get rid of it.

Now that we have laid down the scope of the note, we shall next unfold the topics into a [summary](#technical-summary) of the technical facts, and then give the [type inference details](#detailed-type-inference) which the summary abstracts.

## Technical Summary

In a nut shell, the type-checker reports that there is incompatibility between the inferred type for `tm` as defined in the let-binding (line 2), and the annotated type for `tm` as in the pattern matching (line 6).

It is normal that the type of an expression is specialized to a less general type after it is composed with other expressions and becomes a sub-expression. In our case, the list and the reifier both have some initial type before they become arguments of `Reifier.apply`. Then we `Reifier.apply` the reifier to the list, and this causes the type of both be specialized so that the type of the reifier matches the type of the list. 

For instance, initially the list has type `t1`

```ocaml
t1 ::=

('d Core.ilogic Option.ilogic,
  ('d Core.ilogic,
    ('a, 'b) List.t Core.ilogic)
		       List.t Core.ilogic)
			     List.t Core.ilogic
```
and the reifier has type `t2`
```ocaml
t2 ::=

('e Core.ilogic Option.ilogic List.ilogic, 'e Core.logic Option.logic List.logic) Reifier.t
```
When the reifier is applied to the list, unification of the type expressions specializes the list type `t1` to `t3`
```ocaml
t3 ::=

('g Option.ilogic as 'g) List.ilogic
```
and specializes the reifier type `t2` to `t4`
```ocaml
t4 ::=

(('g Option.ilogic as 'g) List.ilogic,
('g Option.ilogic as 'g) Option.logic Option.logic List.logic) Reifier.t
```

The second argument type
```ocaml
('g Option.ilogic as 'g) Option.logic Option.logic List.logic
```
of `t4` is the inferred type for `tm` in the let-binding, which is obviously incompatible with the annotated type
```ocaml
int Core.logic Option.logic List.logic
```
of `tm` in the pattern matching.

These are the major steps in the manual type analysis, and the resulting incompatibility is just what the type-checker reports. Having understood the error messsage and answered the question "How the invalid list is rejected by the type checker?", we now have some other interesting observations along the way.

 Firstly we used the default polymorphic reifier  `Reifier.reify` for logical integers without explicitly restricting the type to `int`, so we have the type variable `'e` rather than an `int` in `t2`.  If we had added such a restriction, a type error would be
reported one step earlier, not during pattern matching but during the application of the reifier to the list --- actually this (the reifier application step) is where the type error _should_ occur. Unfortunately, due to the lack of explicit type restriction for the reifier for integers, no error occurred at the time when one should occur.  From this we have something to say about the interplay between the reifier's type and the type of the value to be reified. With a precise type for reifiers, invalid values submitted for reification can be timely rejected. Otherwise, if we allow the reifier to have a type more general than it should be, the types of the reifier and the invalid value may unify. In our case the list is recognised as having a fancy recusive member type (`option` of `option` of `option` of ...), and the story is no longer "reifying a wrong value with a right reifier" but becomes "reifying a right value by a wrong reifier". This moves us to the second observation.

The second observation is about using a reifier that is not deep enough. Because the reifier type is not precise, the list type and the reifier type unifies and the list is regarded as a list of infinitely nested options. Members of this nested option type are e.g., `None`, `Some None` , `Some (Some None)`, `Some (Some (Some None))`, ..., and additionally the cyclic term `x = Some x`. If we change the type restriction of `tm` to exactly the list of infinitely nested options (which will also be inferred automatically if we simply remove all the type annotation), we will realize that the reifier (at line 2) for the intended type `int Core.ilogic Option.ilogic List.ilogic` is not deep enough for the actual type `('a Option.ilogic as 'a) List.ilogic`. Applying the reifier in this situation _may_ produce partially reified values which has abstract sub-structures that cannot be pattern matched against. However, it is possible that although the reifier is in general not deep enough for the type, it can still fully reify some values of that type when the depth of the value is within the reach of the reifier. For this reason, if we remove the type restriction in line 6 of the code segment, we will then be able match the term against the pattern and print out a "PASSED".  

## Detailed Type Inference

Consulting relevant module interfaces, and using typing rules for lambda terms,  we have

``` ocaml
(1) (Nil : ('a, 'b) List.t)
(2) (inj : 'c -> 'c Core.ilogic)
(3) ((inj Nil) : ('a, 'b) List.t Core.ilogic)
```
The type for the `v` in `fun v -> ...` is determined by the type of `run`
to be `'d Core.ilogic`, then 

```ocaml
(4) ((Some v) : 'd Core.ilogic option) 
(5) ((inj (Some v)) : 'd Core.ilogic option Core.ilogic) 
```
The interface of `Option` gives the equation 
```ocaml
(6) 'a option Core.ilogic = 'a Option.ilogic
```
Then

```ocaml
(7) ((inj (Some v)) : 'd Core.ilogic Option.ilogic) 
(8) ((Cons(v, (inj Nil))) : ('d Core.ilogic, ('a, 'b) List.t Core.ilogic) List.t)
(9) ((inj (Cons(v, (inj Nil)))) : 
         ('d Core.ilogic, ('a, 'b) List.t Core.ilogic) List.t Core.ilogic)
```
Applying `Cons` to the terms of (7) and (9), and then inject, we get a term of type

```ocaml
(10)  ('d Core.ilogic Option.ilogic, 
         ('d Core.ilogic, 
           ('a, 'b) List.t Core.ilogic) 
		             List.t Core.ilogic) 
					  List.t Core.ilogic
```		   
which is passed to `Env.return` as the argument.

The `run (fun v -> ...)` expression then has a type which is (10) wrapped by `State.t`. 

Next,

```ocaml
(11) (Reifier.reify : ('e Core.ilogic, 'e Core.logic) Reifier.t)
(12) ((Option.reify Reifier.reify) : 
     ('e Core.ilogic Option.ilogic, 'e Core.logic Option.logic) Reifier.t)
(13) ((List.reify (Option.reify Reifier.reify)) : 
     ('e Core.ilogic Option.ilogic List.ilogic, 'e Core.logic Option.logic List.logic) Reifier.t)
```	 
To apply the reifier (13), it requires that the type (14) below (which is the first argument type
of `Reifier.t` in (13)) and the type (10) compatible.

```ocaml
(14) 'e Core.ilogic Option.ilogic List.ilogic
```
We expand the recursive type (14) for several times. let F = (14)
```ocaml
(15) F = ('e Core.ilogic Option.ilogic, F) List.t Core.ilogic 
       = ('e Core.ilogic Option.ilogic, 
          ('e Core.ilogic Option.ilogic, F) List.t Core.ilogic) List.t Core.ilogic
       = ('e Core.ilogic Option.ilogic, 
          ('e Core.ilogic Option.ilogic, 
            ('e Core.ilogic Option.ilogic, F) List.t Core.ilogic) 
                                               List.t Core.ilogic) 
                                                List.t Core.ilogic
       = ...
```
The types (10) and (14) are compitable if (10) unifies with any expanded type expression in (15), and in our case the candidate is the third type expression in (15), which contains the same number (three) of `List.t Core.ilogic` sub-expression as (10). The unification proceeds as follows.

First, `'e Core.ilogic Option.ilogic` unifies `'d Core.ilogic Option.ilogic`, making
the type variables `'e` and `'d` share value. Next, `'e Core.ilogic Option.ilogic` 
(which equals to `'e Core.ilogic Option.t Core.ilogic`) unifies with `'d Core.ilogic`, 
binding `'d` to `'e Core.ilogic Option.t`. Note that `'e` and `'d` share value, then both 
are bound to the same recursive type 

```ocaml
(16) 'f Core.ilogic Option.t as 'f
```
because we allow such recursive types by the compiler option `-rectypes`. Finally, `'a` 
and  `'b` in (10) unify respectively with `'e Core.ilogic Option.ilogic` and F. 
Thus, the result of unification is (i.e., both (10) and (14) become) the type expression
```ocaml
(17) ('f Core.ilogic Option.t as 'f) Core.ilogic Option.ilogic List.ilogic
```

The recursive type (16) expands to 
```ocaml
(18)   ... Option.t) Core.ilogic) Option.t) Core.ilogic) Option.t
```
which grows infinitely from right to left. Starting from the rightmost `Core.ilogic` and
combine it with the `Option.t` to the left, we get an `Option.ilogic`. Continue this process
we get the following type expression that equals to (16)
```ocaml
(19) ('g Option.ilogic as 'g) Option.t
```
Then (17) equals to (substitute (19) back)  
```ocaml
(20) ('g Option.ilogic as 'g) Option.t Core.ilogic Option.ilogic List.ilogic
   = ('g Option.ilogic as 'g) List.ilogic
   = ('g Option.ilogic as 'g) Option.ilogic List.ilogic
```
which is the simplified result of unifying (10) and (14), serving as the first 
argument type of `Reifier.t` in (13), whose  second argument type now is 
(substitute (19) for `'e`)
```ocaml
(21) ('g Option.ilogic as 'g) Option.t Core.logic Option.logic List.logic
   = ('g Option.ilogic as 'g) Option.logic Option.logic List.logic
```

Now we get the type for `tm` in the let-binding, which is just (20). Obviously (20) is incompatible with the type annotation for `tm` in the pattern matching. This is exactly the type error
found by the type checker.



