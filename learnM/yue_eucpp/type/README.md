```ocaml
let reify = fun (ra : ('a ->'b) Env.t)  -> Env.bind Reifier.reify
    (fun r ->
       (Env.bind ra
          (fun fa ->
             Env.return
               (fun x ->
                  match r x with
                  | Var v as v' -> v'
                  | Value t -> Value (fmap fa t)))))
```
