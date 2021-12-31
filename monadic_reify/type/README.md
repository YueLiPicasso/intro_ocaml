# Understanding the monadic reifier

## Non-recursive Reifier

The reifier for the option type is built using the operators "bind" and "return". Without
type annotation, the meaning of the function definition is obscure:
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
It takes some time to manually add type decorations to reveal how this reifier works:
```ocaml
let reify = fun (ra : ('a ->'b) Env.t) ->
  (((Env.bind :
       ('c Core.ilogic -> 'c Core.logic) Env.t
     -> (('c Core.ilogic -> 'c Core.logic) -> ('a ilogic -> 'b logic) Env.t)
     -> ('a ilogic -> 'b logic) Env.t)
      (Reifier.reify : ('c Core.ilogic -> 'c Core.logic) Env.t ))
   : (('c Core.ilogic -> 'c Core.logic) -> ('a ilogic -> 'b logic) Env.t)
   -> ('a ilogic -> 'b logic) Env.t)
    ((fun (r : 'c Core.ilogic -> 'c Core.logic) ->
        ((((((Env.bind : ('a ->'b) Env.t -> (('a ->'b) -> ('a ilogic -> 'b logic) Env.t)
              -> ('a ilogic -> 'b logic) Env.t)
               (ra : ('a ->'b) Env.t))
            : (('a ->'b) -> ('a ilogic -> 'b logic) Env.t) -> ('a ilogic -> 'b logic) Env.t)
             ((fun (fa : 'a -> 'b) ->
                 ((Env.return
                     ((fun (x : 'a t Core.ilogic) ->
                         match (r x : 'a t Core.logic) with
                         | Var v  as v' -> (v' : 'b t Core.logic)  (* Polymorphic Var *)
                         | Value t -> ((Value ((fmap (fa : 'a -> 'b) (t : 'a t)) : 'b t))
                                       : 'b t Core.logic)
                       ) : 'a ilogic -> 'b logic)
                  ) : ('a ilogic -> 'b logic) Env.t)
               ) : ('a ->'b) -> ('a ilogic -> 'b logic) Env.t )
          )) : ('a ilogic -> 'b logic) Env.t)
      ) :  ('c Core.ilogic -> 'c Core.logic) -> ('a ilogic -> 'b logic) Env.t)
```
The decoration also shows that actually the reifier is generic for all non-recursive type constructors of one type parameter. 

## Tips for Type Annotation

* To manually add types for sub-expressions it is [helpful](https://github.com/YueLiPicasso/intro_ocaml/issues/2#issue-1084625874) to draw the tree representation of the expression for easy reference of the expression structure. 
* The type constructor names `logic` and `ilogic` are shared by both the Core and user defined types. Module path qualification is needed to distinguish them for correct type annotation.
* Always make fresh copies of type expressons, e.g., if  `f : 'a -> 'b`, then  use `'a1 -> 'b1` , `'a2 -> 'b2`, `'a3 -> 'b3` etc for different occurrences of `f`.
* Type inference is about making fresh copies of types,  performing unification between type expressions, substituting type variables as per the result of unification, and of course  making use of the typing rules of lambda terms.
* With every extra piece of type annotation added, recompile the code to make sure the existing annotations are compatible with the higher-level type of the function. This does not prevent that in a later satge you may find the whole big and annotated structure break down during machine type checking. In that case, rework from the beginning and watch out for any mistake you have made.

## Recursive Reifier

   
