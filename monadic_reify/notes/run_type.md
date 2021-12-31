# Type Analysis of the Body of `Core.run`

On the level of module interface, `run` is used to generate a state of logical value.

On the level of implementation, `run` generates a logical variable which is then passed to 
the argument function to create a logical value that contains the variable. This value is then paired
with the `env` in which the variable is valid, to form a state.

```ocaml
let run rel = let env = Var.fresh_env () in
              (env, rel (Term.fresh env) env)
```
According to relevant module interfaces,
```ocaml
(1) ((Var.fresh_env ()) : Var.env)
(2) (Term.fresh : Var.env -> 'a Term.t)
(3) ((Term.fresh (Var.fresh_en())) : 'a Term.t)
```

The MGT-Most General Type for parameter `rel` is 
```ocaml
(4) rel : 'a Term.t -> Var.env -> 'b
```
and the MGT for `run` is
```ocaml
(5) (run : ('a Term.t -> Var.env -> 'b) -> Var.env * 'b)
```
that is (with the type equations available)
```ocaml
(6) (run : ('a ilogic -> 'b Env.t) -> 'b State.t)
```
which is then specialized by the sig to

```ocaml
(7) (run : ('a ilogic -> 'c ilogic Env.t) -> 'c ilogic State.t)
```
with `'b` in (6) (the return type of `rel`) specialized to `'c ilogic` in (7). 


