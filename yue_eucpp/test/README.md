# Comments

## How types are respected? - A case study

A typed logical list of the type `int option list` cannot have the form `[Some v;v]` for any loggic variable `v` because `v` cannot at the same time be an `int` (as the argument of `Some`) and an `int option` (as the member of the list). The code below, in which we try to build and reify such an invalid list, causes a type error --- good ! This supports the claim that the monadic reification system is suitable for _typed_ relational programming. 
 
```ocaml
let _ = print_string @@
  let tm = Reifier.apply (List.reify (Option.reify Reifier.reify))
      (run (fun v -> Env.return
               (inj List.(Cons(inj (Some v),
                               inj (Cons(v, (inj Nil)))))))) in
  match (tm : int Core.logic Option.logic List.logic) with
  | Value(Cons(Value(Some(Var _)), Value (Cons (Value None, Value Nil))))
    -> "PASSED\n"
  | _ -> "failed\n"
```
In a nut shell, the type error reported
by the type-checker is the incompatibility between the inferred type for `tm` as defined in the
let-binding, and the annotated type for `tm` as in the pattern matching. Now we see in detail how the type error occurs.

### Let's take a manual type inference exercise !

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

The `run (fun v -> ...)` expression then has a type which is (10) wrapped by `State.t`. Next,

```ocaml
(11) (Reifier.reify : ('e Core.ilogic, 'e Core.logic) Reifier.t)
(12) ((Option.reify Reifier.reify) : 
     ('e Core.ilogic Option.ilogic, 'e Core.logic Option.logic) Reifier.t))
(13) ((List.reify (Option.reify Reifier.reify)) : 
     ('e Core.ilogic Option.ilogic List.ilogic, 'e Core.logic Option.logic List.logic) Reifier.t))
```	 
To apply the reifier (13), it requires that the type (14) (below, which is the first argument type
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
argument type of `Reifier.t` in (13), whose  second  argument type now is 
(substitute (19) for `'e`)
```ocaml
(21) ('g Option.ilogic as 'g) Option.t Core.logic Option.logic List.logic
   = ('g Option.ilogic as 'g) Option.logic Option.logic List.logic
```
Now we get the type for `tm` in the let-binding, which is just (20). Obviously (20) is incompatible with the type annotation for `tm` in the pattern matching.




