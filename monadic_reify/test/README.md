- [Type Safety Case study](#how-types-are-respected---a-case-study)
- [Using Multiple Logical Variables](#introducing-multiple-distinct-logical-variables)
- [Solving the Problem of Non-terminating Monadic Reifiers for Certain Recursive Types](#solving-the-problem-of-non-terminating-monadic-reifiers-for-certain-recursive-types)

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

It is good to have the error found by the type-checker because it gives confidence over type-safty of monadic reifiers. The question then is "how is this invalid list rejected by the type checker?". To answer this question is to understand the error message. For me the error message does not make sense after a first reading so I massaged through the type inference process manually and as I hoped I found the same type clash as reported by the type-checker. Along the way I also noticed
several interesting aspects of the code:


- The interplay between the reifier's type and the type of the term to be reified.
- The consequence of using a reifier that is not deep enough.

Next, in the [Type Clash Summary](#type-clash-summary) I give the major pieces of type information that lead to the type error; and then in [Bonus Observations](#bonus-observations) I discuss on interesting facts and further experiments revealed/motivated by the manual type inference. Finally, in  [Detailed Type Inference](#detailed-type-inference) I show the type inference process in relatively small steps.

## Type Clash Summary

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

These are the major steps in the manual type analysis, and the resulting incompatibility is just what the type-checker reports. Step-by-step type inference is given in [Detailed Type Inference](#detailed-type-inference).

## Bonus Observations

Having understood the error messsage and answered the question "How the invalid list is rejected by the type checker?", we now have some other interesting observations along the way.

 Firstly we used the default polymorphic reifier  `Reifier.reify` for logical integers without explicitly restricting the type to `int`, so we have the type variable `'e` rather than an `int` in `t2`.  If we had added such a restriction, a type error would be
reported one step earlier, not during pattern matching but during the application of the reifier to the list --- actually this (the reifier application step) is where the type error _should_ occur. Unfortunately, due to the lack of explicit type restriction for the reifier for integers, no error occurred at the time when one should occur.  From this we have something to say about the interplay between the reifier's type and the type of the value to be reified. With a precise type for reifiers, invalid values submitted for reification can be timely rejected. Otherwise, if we allow the reifier to have a type more general than it should be, the types of the reifier and the invalid value may unify. In our case the list is recognised as having a fancy recusive member type (`option` of `option` of `option` of ...), and the story is no longer "reifying a wrong value with a right reifier" but becomes "reifying a right value by a wrong reifier". This moves us to the second observation.

The second observation is about using a reifier that is not deep enough. Because the reifier type is not precise, the list type and the reifier type unifies and the list is regarded as a list of infinitely nested options. Members of this nested option type are e.g., `None`, `Some None` , `Some (Some None)`, `Some (Some (Some None))`, ..., and additionally the cyclic term `x = Some x`. If we change the type restriction of `tm` to exactly the list of infinitely nested options (which will also be inferred automatically if we simply remove all the type annotation), we will realize that the reifier (at line 2) for the intended type `int Core.ilogic Option.ilogic List.ilogic` is not deep enough for the actual type `('a Option.ilogic as 'a) List.ilogic`. Applying the reifier in this situation _may_ produce partially reified values which has abstract sub-structures that cannot be pattern matched against. However, it is possible that although the reifier is in general not deep enough for the type, it can still fully reify some values of that type when the depth of the value is within the reach of the reifier. For this reason, if we remove the type restriction in line 6 of the code segment, we will then be able to match the term against the pattern and print out a "PASSED". 


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

Now we get the type for `tm` in the let-binding, which is just (21). Obviously (21) is incompatible with the type annotation for `tm` in the pattern matching. This is exactly the type error
found by the type checker.


# Introducing Multiple Distinct Logical Variables

One variable is introduced by `run`, but how to introduce two or more distinct logical variables? For a single variable `v`, `[Some v; v]` can never be a logical list of integer options, but for two distinct variables `v1, v2`, `[Some v1; v2]` can be a logical list of integer options, where `v1` is a logical integer and `v2` is a logical integer option.

If we write 
```ocaml
run (fun v1 -> fun v2 -> List.cons (Option.some v1) (List.cons v2 (List.nil())))
```
then we would read (according to the definition of `run`) that `run` generates a fresh environment `env` and applies the function `fun v1 -> fun v2 -> ...` to a freshly generated logical variable and `env`. Here `v1` would be bound to the variable but `v2` would be bound to the `env`, which is wrong. We need to  insert something in between `fun v1 ->` and `fun v2 ->` in order to catch  the `env` and meanwhile generate a second variable. `fresh` does this for us
```ocaml
run (fun v1 -> fresh (fun v2 -> List.cons (Option.some v1) (List.cons v2 (List.nil()))))
```
However, `fresh` applies `fun v2 -> ...` to a new variable and `env`, but this application is not possible because that after `v2` is bound to the new variable, the `<body>` of `fun v2 -> <body>` can accept no `env`. This problem is solved if we apply `Env.return` to the `<body>`
```ocaml
run (fun v1 -> fresh (fun v2 -> Env.return @@ 
  List.cons (Option.some v1) (List.cons v2 (List.nil()))))
```
Now the types are correct and the meaning of the expression is also correct. If we rewrite the expression using the definitions of relevant functions, we would see that `run` creates an `env` and use it to create a fresh variable. `fun v1 -> ...` is applied to the variable and `env` so that `v1` is bound to the variable and `fresh (fun v2 -> ...)` is applied to the `env`. Then `fresh` uses this `env` to create a second variable, to which and to `env` the function `fun v2 -> ...` is applied, so that `v2` is bound to the second variable and `Env.return @@ List.cons ...` is applied to `env`. Finally only 
```ocaml
List.cons (Option.some v1) (List.cons v2 (List.nil()))
```
is returned with `v1` and `v2` bound to distinct logical variables (both created wrt. the same `env`) --- this is exactly what we want.

# Solving the Problem of Non-terminating Monadic Reifiers for Certain Recursive Types

- [Motivation](#motivation)
- [Showing the Problem](#showing-the-problem)
- [Summary of the Difficulties](#summary-of-the-difficulties)
- [Idea of Solution](#idea-of-solution)
- [Result](#result)
- [Discussion](#discussion)

## Motivation

We [saw](#how-types-are-respected---a-case-study) that the infinitely nested option type `'a Option.ilogic as 'a` is inferred for members of the logical list `[Some v; v]` when we tried to reify it by composing the list reifier `List.reify`, the option reifier `Option.reify` and the default shallow reifier `Reifier.reify` without any additional type annotation. We also saw that the so composed reifier is in general too shallow for arbitrary values of the type of list of infinitely nested options. We ask: can we write a reifier for the type `'a Option.ilogic as 'a` ? An experienced relational programmer would also see here the structural identity between the type of infinitely nested options and the type of natural numbers represented as Peano numerals. You may think that since Peano numerals are so common in relational programming, there shouldn't be any problem writing a reifier for it and for  any type that is homomorphic to it. The reality is, however, we face a succession of problems when trying to write such a reifiier in the monadic style.

First let's define the type for infinitely nested optoins in the module `Opnest` and see its structural identity with Peano numerals defined in the module `Penum`. 

```ocaml
module Option = struct
  type 'a t      = 'a option
  type 'a logic  = 'a t Core.logic
  type 'a ilogic = 'a t Core.ilogic
end

module Opnest = struct
  type logic  = 'a Option.logic  as 'a
  type ilogic = 'a Option.ilogic as 'a
end
```

Note that 
```ocaml
type 'a option = None | Some of 'a
type 'a peano  = Zero | Succ of 'a

module Peano = struct
  type 'a t = 'a peano
  type 'a logic = 'a t Core.logic
  type 'a ilogic = 'a t Core.ilogic
end

module Penum = struct
  type logic  = 'a Peano.logic as 'a
  type ilogic = 'a Peano.ilogic as 'a
end
```
Therefore as `Opnest` admits values like
```ocaml
None, Some None, Some (Some None), Some (Some (Some None)), ...
x = Some x
``` 
`Penum` admits values like
```ocaml
Zero, Succ Zero, Succ (Succ Zero), Succ (Succ (Succ Zero)), ...
x = Succ x
```
## Showing the Problem

Following the model of list reifier, we can easily fabric a plausible reifier for `Opnest`
```ocaml
(* Opnest.reify, version 1 *)

let rec reify = Reifier.compose Reifier.reify 
      (Env.bind reify (fun r ->
           Env.return (fun x ->
               match x with
               | Var _ as v' -> v'
               | Value t -> Value (fmap r t))))
``` 
Here, unfortunately, OCaml would see an invalid recursive definition of a non-functional value `reify`, and more precisely, the body of the definition is not [_staticly constructive_](https://ocaml.org/releases/4.11/htmlman/letrecvalues.html) with respect to the name `reify` being defined.
We may come up with the simple remedy of making `reify` accept a `unit` value as the first argument
```ocaml
(* Opnest.reify, version 2 *)

let rec reify () = Reifier.compose Reifier.reify 
      (Env.bind (reify ()) (fun r ->
           Env.return (fun x ->
               match x with
               | Var _ as v' -> v'
               | Value t -> Value (fmap r t))))
```
However, although the second version is acceptable for the compiler, at runtime calling this reifier would always trigger an infinite loop that overflows the stack, even when the value to be reified is _not_ cyclic. This is because the evaluation of `reify()` depends on a cyclic evaluation of `reify()` itself, and `reify()` must be evaluated before it is `Reifier.apply`ed due to the call-by-value rule of OCaml. In consequence, this reifier dies in a loop before it ever meets the value to be reified. We may ask: "I remember that a reifier usually takes an `env` as the first argument, can we just write
```ocaml
(* Opnest.reify, version 3 *)

let rec reify = fun env -> Reifier.compose Reifier.reify 
      (Env.bind reify (fun r ->
           Env.return (fun x ->
               match x with
               | Var _ as v' -> v'
               | Value t -> Value (fmap r t)))) env
```
?" If we do so, we would immediately receive a type error, saying that `Reifier.compose` is given
 too many arguments.This is down to the bottom because the enviroment monad `Core.Env` hides the type equation `'a Env.t = Var.env -> 'a`, so that as a user of the `Core`, the `Opnest` 
library can only see `('a, 'b) Reifier.t = ('a -> 'b) Env.t` but cannot see that `('a -> 'b) Env.t = Var.env -> 'a -> 'b`. A non-monadic reifier of course exists
```ocaml
(* Opnest.reify, version 4 *)

let rec reify env = fun x -> match (Reifier.reify env x) with
             | Var _ as v' -> v'
             | Value t -> Value (fmap (reify env) t) 
```
but again it cannot be typed under the monad paradigm: it lives in a totally different world with
 totally different type management.

## Summary of the Difficulties

- Failure of version 1 taught us that we must define `Opnest.reify` to be staticly constructive.
- Failure of version 2 taught us that taking a `unit` argument does not help, for it passes the static check but loops at runtime.
- Failure of versions 3 and 4 taught us that we have no retreat into the practice of non-monadic programming in the world of monads.  

## Idea of Solution

Static constructiveness is not just about using `fun ...-> ...` but also `lazy (...)` as per the OCaml manual. We may use OCaml lazy evaluation feature to write a lazy reifier and write a new monadic binder for the lazy reifier. The lazy evaluation prevents `reify` itself from looping when it is evaluated as an argument of `Reifier.apply`. The new binder simply forces the suspension in addtion to doing what a usual binder does. 

## Result

Add sub-module `Lazy` to `Core.Env`

```ocaml
module Lazy = struct
    let bind r k env = k (Lazy.force r env) env
end
```
Take our `Opnest.reify` version 1, wrap the whole thing in `lazy`, and use the module path `Env.Lazy` instead of `Env` in `Env.bind`

```ocaml
(* Opnest.reify, version 5 *)

let rec reify = lazy (Reifier.compose Reifier.reify 
      (Env.Lazy.bind reify (fun r ->
           Env.return (fun x ->
               match x with
               | Var _ as v' -> v'
               | Value t -> Value (fmap r t))))) 
```
That's all. And it has type 
```ocaml
(ilogic, logic) Reifier.t Lazy.t
```
Evaluation of `refiy` itself always terminates because of lazyness, and when being applied it penetrates any number of layers of nested `option`s becasue the looping power is released by the `Env.Lazy.bind` precisely on the value to be reified, instead of being released prematurely which is the case of the earlier unsuccessful version 2. This solution overcomes all the listed difficulties.
 
## Discussion 

Regarding the proposed solution: 

- It does not loop at runtime for non-cyclic values. For cyclic values it loops (of course).
- Created by simple modification of a naive monadic reifier.
- It can be composed with other reifiers as usual when wrapped by `Stdlib.Lazy.force`.
- Monad abstraction is respected, the implementation of `Env.t` remains abstract.
- It is not specific to nested options. It may be automated by a generic programming framework.
- [Not thread-safe](https://ocaml.org/releases/4.11/htmlman/libref/Lazy.html).

The reifier is also implemented using some [fix-point concept](https://github.com/Kakadu/OCanren/blob/dce7c390559e273cb25589b7b672291b28c742a3/src/core/Logic.ml#L106) at the cost of [breaking](https://github.com/Kakadu/OCanren/blob/dce7c390559e273cb25589b7b672291b28c742a3/src/core/Env.mli#L44) the monad abstraction. I guess that my colleague met the same problem of non-termination when experimenting with the naive monadic reifier on Peano numbers. As a first remedy he may defined the fix-point operator but soon realized that it does not help if the `Env.t` type remains abstract. Maybe he also have noticed that the advantage (in terms  of avoiding a predefined set of module functors) of monadic reification over the [old style](https://github.com/JetBrains-Research/OCanren/blob/8ce216180e2abe37b8a1f60cf6bf9187c63fc81c/src/core/Logic.ml#L139) is essentially not because of monad but because of the definition of `ilogic` types,  therefore he decided to pick out the `ilogic` type and compromise the monad abstraction, so that he could have staticly constructiveness in the form of `fun...-> ...`. 










