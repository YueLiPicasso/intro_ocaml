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