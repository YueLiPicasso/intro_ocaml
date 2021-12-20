# Reimplementation of OCanren Reifiers using Light-weight Higher-kinded Polymorphism

OCanren [reifiers](https://github.com/YueLiPicasso/OCanrenTutorial/blob/d4a7cdb198900bf19e21354c1d2bdddf15f4391a/Installation/ocanren/src/core/Logic.mli#L132) are originally defined using functors. Now we use the [_Light-weight Higher-kinded Polymorphism_](https://doi.org/10.1007/978-3-319-07151-0_8) technique to define them without functors. 


## To Run the Code

Just load it to the OCaml REPL.

## Special Feature

Not just an application of the "light-weight higher-kinded polymorphism" technique, but also 
addresses a limitation of it: not applicable for defining recursive types. 
```ocaml
type 'a t = ('a, 'b) app as 'b
```
would be rejetced because `app` is abstract. To work around, give `app` a dummy representation, such as
```ocaml
type ('a, 'b) app = App of 'a * 'b
```
