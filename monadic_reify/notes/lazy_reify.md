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