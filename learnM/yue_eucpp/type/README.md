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
