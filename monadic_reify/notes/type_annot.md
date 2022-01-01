# Understanding the Monadic Reifiers

For `('a1, ..., 'ak) SomeCustomType.ilogic (k >= 0)` that is defined based on  `('a1, ..., 'an) SomeCustomType.t (n >= 0)` and `Core.ilogic`, a monadic reifier is built using 

- the shallow reifier `Reifier.reify`, and if `SomeCustomType.t` is an algebraic data type with at least one non-constant value constructor,
- the usual monadic operators `Env.return` and `Env.bind`, and `SomeCustomType.fmap : ('a1 -> 'b1) -> ... -> ('an -> 'bn) -> ('a1, ..., 'an) t -> ('b1, ..., 'bn) t` , and if `SomeCustomType.ilogic` is recursively defined, additionally
- the reifier composer `Reifier.compose`.

We shall discuss

- [The Shallow Reifier](#the-shallow-reifier)
- [Non-recursive Reifiers](#non-recursive-reifiers)
    - [Operational Details](#operational-details)
    - [The Monad Abstraction](#the-monad-abstraction)
    - [Typing Details](#typing-details)
- [Recursive Reifiers](#recursive-reifiers)
- [Tips for Type Annotation](#tips-for-type-annotation)

## The Shallow Reifier

The shallow reifier `Reifier.reify` is defined to be equal to

```ocaml
(* In the module - Core *)

let observe : Var.env -> 'a ilogic -> 'a logic =
  fun env t -> match Term.var env t with
               | None -> Value (Obj.magic t)
               | Some v -> Var v
```
and has a type `('a Core.ilogic, 'a Core.logic) Reifier.t` for the user, which hides the `Var.env` detail. 

The most general type  (MGT) inferred for `observe` is `Var.env -> 'a Core.ilogic -> 'b Core.logic`. The technical type of `observe` is the MGT manually specialized to synchronize `'a` and `'b` for type bookkeeping purposes. The shallow reifier performs the smallest possible step of reification: it decides if a value passed to it is a logical variable of not, and applies a wrapper accordingly. 

## Non-recursive Reifiers

The reifier for the option type is studied as a typical non-recursive reifier. 
```ocaml
 let reify = fun ra ->
    let (>>=) = Env.bind in
    (Reifier.reify >>= (fun r -> (ra >>= (fun fa ->
      Env.return (fun x ->
        match r x with
        | Var v   -> Var v
        | Value t -> Value (fmap fa t)
      )))))
```

### Operational Details

If the operational meaning of the monadic reifier is not obvious for you, it is worth to expand the monad operators `Env.bind` and `Env.return` by their definitions and then simplify to get an equivalent non-monadic reifier which may give you a clearer idea of what the monadic reifier does. Our monadic reifier is so transformed to the following   
```
ra  : reifier for the argument type 'a of 'a option
env : abstract variable environment 

reify ra env = fun x ->
        match Reifier.reify env x with
        | Var _ as v'  -> v'
        | Value t -> Value (fmap (ra env) t)
```
We can read that the option reifier takes as arguments a sub-reifier `ra`,  an `env` and the value `x` to be reified. The shallow reifier `Reifier.reify` is used upfront to process `x`, then the control branches on the shape of the result of shallow reification.


The role played by the shallow reifier is remarkable. If `x` is a variable, the shallow reification result is directedly returned. If `x` is not a variable, the shallow reifier by definition would peform a type cast on `x` from `<some type> Core.ilogic` to just `<some type>`, then wrap `x` in the constructor `Value`. In both cases the result has type `<some type> Core.logic`.  

### The Monad Abstraction

Since a user never needs to work with values of the type `Var.env` directly (e.g., creating values of this type and supplying them as arguments in function calls), we don't even have to mention this type to the user, otherwise they would be distracted. This means that we shall disallow the occurrence of the type expression `Var.env` in the user interface, while passing around `env`s under the hood. This is realized by monad abstraction.

 Comparing the two presentations above, we would see that e.g., the binder `>>=` that relates `Reifier.reify` and `fun r -> ...` secretly supplies an `env` to `Reifier.reify` and then (functionally) binds the result to `r`, i.e., occurrences of `r` in the `<body>` of `fun r -> <body>` are all replaced by `Reifier.reify env`. Using the monad abstraction, we just think "bind `Reifier.reify` to `r`, then bind `ra` to `fa`", which is shorter and less confusing than "bind `Reifier.reify env` to `r`, then bind `ra env` to `fa` and we do not have to care what `env` is."

### Typing Details

 Without type annotation we would miss a significant perspective of how the reifier works. 
```ocaml
let reify =
    ((fun (ra : ('a,'b) Reifier.t) ->
        (((((Env.bind :
               ('c Core.ilogic, 'c Core.logic) Reifier.t
             -> (('c Core.ilogic -> 'c Core.logic) -> ('a ilogic, 'b logic) Reifier.t)
             -> ('a ilogic, 'b logic) Reifier.t)
              (Reifier.reify : ('c Core.ilogic, 'c Core.logic) Reifier.t ))
           : (('c Core.ilogic -> 'c Core.logic) -> ('a ilogic, 'b logic) Reifier.t)
           -> ('a ilogic, 'b logic) Reifier.t)
            ((fun (r : 'c Core.ilogic -> 'c Core.logic) ->
                ((((((Env.bind
                      : ('a, 'b) Reifier.t
                      -> (('a -> 'b) -> ('a ilogic, 'b logic) Reifier.t)
                      -> ('a ilogic, 'b logic) Reifier.t)
                       (ra : ('a, 'b) Reifier.t))
                    : (('a -> 'b) -> ('a ilogic, 'b logic) Reifier.t)
                    -> ('a ilogic, 'b logic) Reifier.t)
                     ((fun (fa : 'a -> 'b) ->
                         ((Env.return
                             ((fun (x : 'a ilogic) ->
                                 match (r x : 'a logic) with
                                 | Var _  as v' -> (v' : 'b logic)   (* Polymorphic Var *)
                                 | Value t ->
                                   ((Value ((fmap (fa : 'a -> 'b) (t : 'a t)) : 'b t))
                                    : 'b logic))
                              : 'a ilogic -> 'b logic))
                          : ('a ilogic, 'b logic) Reifier.t))
                      : ('a ->'b) -> ('a ilogic, 'b logic) Reifier.t )))
                 : ('a ilogic, 'b logic) Reifier.t))
             : ('c Core.ilogic -> 'c Core.logic) -> ('a ilogic, 'b logic) Reifier.t))
         : ('a ilogic, 'b logic) Reifier.t))
     : ('a,'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t)
```


## Recursive Reifiers

The reifier for the infinitely nested option type is studied as a typical recursive reifier. 

```ocaml
let rec reify = lazy (Reifier.compose Reifier.reify 
      (Env.Lazy.bind reify (fun r ->
           Env.return (fun x ->
               match x with
               | Var _ as v' -> v'
               | Value t -> Value (fmap r t)))))
```

Here our main interest is at the typing details.


```ocaml
let rec reify =
      ((lazy
         (((((Reifier.compose :
                (ilogic, ilogic t Core.logic) Reifier.t
              -> (ilogic t Core.logic, logic) Reifier.t
              -> (ilogic, logic) Reifier.t)
               (Reifier.reify :
                  (ilogic, ilogic t Core.logic) Reifier.t))
            : (ilogic t Core.logic, logic) Reifier.t -> (ilogic, logic) Reifier.t)
             (((((Env.Lazy.bind
                  : (ilogic, logic) Reifier.t Lazy.t
                  -> ((ilogic ->  logic) -> (ilogic t Core.logic, logic) Reifier.t)
                  -> (ilogic t Core.logic, logic) Reifier.t)
                   (reify : (ilogic, logic) Reifier.t Lazy.t))
                : ((ilogic ->  logic) -> (ilogic t Core.logic, logic) Reifier.t)
                -> (ilogic t Core.logic, logic) Reifier.t)
                 ((fun (r : ilogic ->  logic) ->
                     (((Env.return
                        : (ilogic t Core.logic -> logic)
                        -> (ilogic t Core.logic, logic) Reifier.t)
                         ((fun (x : ilogic t Core.logic) ->
                             match x with
                             | Var _ as v' -> (v' : logic)
                             | Value (t : ilogic t) ->
                               ((Value ((fmap (r : ilogic -> logic) (t : ilogic t)) : logic t))
                                : logic))
                          : ilogic t Core.logic -> logic))
                      : (ilogic t Core.logic, logic) Reifier.t))
                  : (ilogic ->  logic) -> (ilogic t Core.logic, logic) Reifier.t))
              : (ilogic t Core.logic, logic) Reifier.t))
          : (ilogic, logic) Reifier.t))
       : (ilogic, logic) Reifier.t Lazy.t)
```
## Tips for Type Annotation

* To manually add types for sub-expressions it is [helpful](https://github.com/YueLiPicasso/intro_ocaml/issues/2#issue-1084625874) to draw the tree representation of the expression for easy reference of the expression structure. 
* The type constructor names `logic` and `ilogic` are shared by both the Core and user defined types. Module path qualification is needed to distinguish them for correct type annotation.
* Always make fresh copies of type expressons, e.g., if  `f : 'a -> 'b`, then  use `'a1 -> 'b1` , `'a2 -> 'b2`, `'a3 -> 'b3` etc for different occurrences of `f`.
* Type inference is about making fresh copies of types,  performing unification between type expressions, substituting type variables as per the result of unification, and of course  making use of the typing rules of lambda terms.
* With every extra piece of type annotation added, recompile the code to make sure the existing annotations are compatible with the higher-level type of the function. This does not prevent that in a later satge you may find the whole big and annotated structure break down during machine type checking. In that case, rework from the beginning and watch out for any mistake you have made.
