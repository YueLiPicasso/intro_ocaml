# Comments

## How types are respected?

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
The above code has type error --- good ! (`v` get incompatiable types) 

``` ocaml
(1) (Nil : ('a, 'b) List.t)
(2) (inj : 'c -> 'c Core.ilogic)
(3) ((inj Nil) : ('a, 'b) List.t Core.ilogic)
```

The type for the `v` in `fun v -> ...` is determed by the type of `run`
to be `'d Core.ilogic`, then 

```ocaml
(4) ((Some v) : ('d Core.ilogic) option) 
(5) ((inj (Some v)) : ('d Core.ilogic) option Core.ilogic) 
```
The interface of `Option` gives the equation 
```ocaml
(6) 'a option Core.ilogic = 'a Option.ilogic
```
Then

```ocaml
(7) ((inj (Some v)) : ('d Core.ilogic) Option.ilogic) 
(8) ((Cons(v, (inj Nil))) : ('d Core.ilogic, ('a, 'b) List.t Core.ilogic) List.t)
(9) ((inj (Cons(v, (inj Nil)))) : 
         ('d Core.ilogic, ('a, 'b) List.t Core.ilogic) List.t Core.ilogic)
```
Applying `Cons` to then terms of (7) and (9), and then inject, we get a term of type

```ocaml
(10)  (('d Core.ilogic) Option.ilogic, 
         ('d Core.ilogic, 
           ('a, 'b) List.t Core.ilogic) List.t Core.ilogic) List.t Core.ilogic
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
Applying the above reifier (13) requires that the types (14) (below) and (10) compatible.

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
```
The diffifculty of unifying (10) and (15) lies at unifying `'e Core.ilogic Option.ilogic` which
equals `'e Core.ilogic Option.t Core.ilogic`, with `'d Core.ilogic Option.ilogic` and then
with `'d Core.ilogic`.



