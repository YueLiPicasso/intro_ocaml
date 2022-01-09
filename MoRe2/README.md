# Reifiers Written in the Monad Pattern

The [MoRe](../MoRe) project distinguishes lazy and eager monadic reifiers, so that a user of the reifiers has to care about if a reifier is lazy or not, and accordingly choose which binder (`>>=` or `>>>=`) and which application function (`Reifier.apply` or `Reifier.Lazy.apply`) to use. Moreover, although it is possible to mix-compose lazy and eager reifiers, this again requires the user to take care of wrapping the lazy reifiers with `Lazy.force`.  Here in MoRe2 we free the user from the burdens associated with lazy evaluation. We use a sum type to unify lazy and eager reifiers. 

The benefits are 
- We only need one monadic binder and one application function, rather than two;
- When composing reifiers we do not need to care about if they are lazy or eager. 

The costs are
- Introducing a simple sum type. 
- When implementing a reifier, additionally wrap the definition body with one of the constructors `Lazy` and `Eager` of the sum type.


## To Build

Use either Dune or GNU Make.

### Dune
```
dune build
dune runtest
``` 

### GNU Make
```
make
make runtest
```

## Technical Details

The migration from MoRe to MoRe2 involves no significant structural change of the code. MoRe2 is essentially a paraphrase of MoRe. For instance, in MoRe we have two binders:
```ocaml
(* MoRe *)
val Env.bind      : 'a t        -> ('a        -> 'b t) -> 'b t
val Env.Lazy.bind : 'a t Lazy.t -> ('a Lazy.t -> 'b t) -> 'b t
```
We can unify `'a t`  and  `'a t Lazy.t` (and similarly unify `'a` and `'a Lazy.t`) under the MoRe2 sum type 
```ocaml
(* MoRe2 *)
module EL = stuct
  type 'a t = Eager of 'a
            | Lazy  of 'a Lazy.t
end
```
so that we only need one binder for both lazy and eager types
```ocaml
(* MoRe2 *)
val Env.bind : 'a t EL.t -> ('a EL.t -> 'b t) -> 'b t 
```
where a value of type `'a t EL.t` has the shape `Eager of 'a t`   or   `Lazy of 'a t Lazy.t`. When binding `a` and `b`, under MoRe 
the user needs to know that `a` is eager and write `a >>= b`, or `a` is lazy and write `a >>>= b`. But under MoRe2 the user doesn't
 care if `a` is lazy or eager, and he just writes `a >>= b` and the implementation of `>>=` takes care of the cases
```ocaml
(* MoRe2, module Env *)
let bind r k env =
    let re = match r with
      | EL.Eager r ->  EL.Eager (r env)
      | EL.Lazy  r ->  EL.Lazy (lazy (Lazy.force r env))
    in k re env
```
which just combines the implementations of eager and lazy bind of MoRe
```ocaml
(* MoRe, module Env *)
let bind r k env = k (r env) env
(* module Env.Lazy *) 
let bind r k env = k (lazy (Lazy.force r env)) env
```

Reifier types lazy and eager can also be unified similarly. In MoRe a user works with `('a, 'b) Reifier.t = ('a -> 'b) Env.t` and `('a, 'b) Reifier.t Lazy.t = ('a -> 'b) Env.t Lazy.t` but in MoRe2 the user works with just `('a, 'b) Reifier.t = ('a -> 'b) Env.t EL.t` which has two value shapes `Eager of ('a -> 'b) Env.t` and `Lazy of ('a -> 'b) Env.t Lazy.t` that have clear correspondence in MoRe.   


Now the implementer of reifiers need to clearly wrap a reifier by as being `Lazy` or `Eager`. For instance, previously
```ocaml
Reifier.reify = observe
```
but now 
```ocaml
Reifier.reify = EL.Eager observe
```
And, for one more comparison, previously
```ocaml
(* MoRe, library List *)

let rec reify =
  fun ra -> lazy
    (Reifier.reify >>= (fun r -> ra >>= (fun fa -> reify ra >>>= (fun fr ->
         Env.return (fun x -> match r x with
             | Var _ as v -> v
             | Value t -> Value (fmap fa (Lazy.force fr) t)))))) 
```
where we need both `>>=` and `>>>=`  but now
```ocaml
(* MoRe2, library List *)
let rec reify = fun ra -> EL.Lazy
    (lazy (Reifier.reify >>= (fun r -> ra >>= (fun fa -> reify ra >>= (fun fr ->
         Env.return (fun x -> match EL.use r x with
             | Var _ as v -> v
             | Value t -> Value (fmap (EL.use fa) (EL.use fr) t)))))))  
```
where we need only `>>=`.


## Tips

A functional programmer is not born with an understanding of category theory but is very likely to be tempted 
to look at this field when being repeatedly confronted by codes that
feature the "monad" programming pattern, because monad origins from category theory (See [Wadler](https://homepages.inf.ed.ac.uk/wadler/topics/monads.html)). On the one hand, I tried several available resources on category but only to realise that there is no answer quick and good for the question "What is category, what is monad and why they are interesting?", and at the moment I still do not have a grasp of the field. On the other hand, I find it helpful to build a mental firewall in my head between "functional programs in the monad pattern" and "monad as part of category theory", so that the curiosity inspired by the former for the latter is kept in check, and to be content with looking at these programs solely as pieces of ordinary definitions to be read and understood as is and without the categorical canotations. 


## Credits

- The [MoRe](../MoRe) project
- The [monadic_reify](../monadic_reify) project
- The [yue_eucpp](../yue_eucpp) project 
- The [Moiseenko](https://gist.github.com/eupp/a78e9fc086834106e98d50e1e7bdea24) project
