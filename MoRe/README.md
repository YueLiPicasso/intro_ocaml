# Reifiers Written in the Monad Pattern

Recursive reifiers in the [Moiseenko](https://gist.github.com/eupp/a78e9fc086834106e98d50e1e7bdea24) project are written using a `compose` operator to avoid looping. The [yue_eucpp](../yue_eucpp) project finds that the `compose` operator fails to prevent looping in some important cases. The problem is then solved by the [monadic_reify](../monadic_reify) project using lazy evaluation. Here we get rid of the `compose` operator entirely, and use just lazy evaluation to prevent looping in all cases in interest. The benefit in so doing is further explained bellow. 

The lazy monadic reifier for list which we propose looks like
```ocaml
let rec reify :  ('a, 'b) Reifier.t Lazy.t -> ('a List.ilogic, 'b List.logic) Reifier.t Lazy.t =
  fun ra -> lazy
    (Reifier.reify >>= (fun r -> ra >>>= (fun fa -> reify ra >>>= (fun fr ->
         Env.return (fun x -> match r x with
             | Var _ as v -> v
             | Value t -> Value (fmap (Reifier.Lazy.force fa) (Reifier.Lazy.force fr) t)))))) 
```
But in the [Moiseenko](https://gist.github.com/eupp/a78e9fc086834106e98d50e1e7bdea24) project, the list reifier looks like 
```ocaml
let rec reify : ('a, 'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t = fun ra ->
    Reifier.compose Reifier.reify @@
      (ra >>= (fun fa -> (reify ra >>= (fun fr ->
        Env.return (fun lx ->
          match lx with
          | Var v   -> Var v
          | Value t -> Value (fmap fa fr t))))))
```
where the `compose` operator obscures the monadic semantics of the reifier definition. 

Moreover, we have a working reifier for the type of nested options (which is homomorphic to the type of Peano numbers)
```ocaml
let rec reify = lazy
    (Reifier.reify >>= (fun r -> reify >>>= (fun rr -> Env.return (fun x ->
         match r x with
         | Var _ as v -> v
         | Value t -> Value (fmap (Reifier.Lazy.force rr) t)))))
```  
This has no parallel in the [Moiseenko](https://gist.github.com/eupp/a78e9fc086834106e98d50e1e7bdea24) project.

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
## Tips

A functional programmer is not born with an understanding of category theory but is very likely to be tempted 
to look at this field when being repeatedly confronted by codes that
feature the "monad" programming pattern, because monad origins from category theory (See [Wadler](https://homepages.inf.ed.ac.uk/wadler/topics/monads.html)). On the one hand, I tried several available resources on category but only to realise that there is no answer quick and good for the question "What is category, what is monad and why they are interesting?", and at the moment I still do not have a grasp of the field. On the other hand, I find it helpful to build a mental firewall in my head between "functional programs in the monad pattern" and "monad as part of category theory", so that the curiosity inspired by the former for the latter is kept in check, and to be content with looking at these programs solely as pieces of ordinary definitions to be read and understood as is and without the categorical canotations. 


## Credits

- The [monadic_reify](../monadic_reify) project
- The [yue_eucpp](../yue_eucpp) project 
- The [Moiseenko](https://gist.github.com/eupp/a78e9fc086834106e98d50e1e7bdea24) project
