# Reifiers Written in the Monad Pattern

The [MoRe](../MoRe) project distinguishes lazy and eager monadic reifiers, so that a user of the reifiers has to care about if a reifier is lazy or not, and accordingly choose which binder (`>>=` or `>>>=`) and which application function (`Reifier.apply` or `Reifier.Lazy.apply`) to use. Moreover, although it is possible to mix-compose lazy and eager reifiers, this again requires the user to take care of wrapping the lazy reifiers with `Lazy.force`.  Here in MoRe2 we free the user from the burdens associated with lazy evaluation. We use a sum type to unify lazy and eager reifiers. The benefits are 
- We only need one monadic binder and one application function, rather than two;
- When composing reifiers we do not need to care about if they are lazy or eager. 

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

- The [MoRe](../MoRe) project
- The [monadic_reify](../monadic_reify) project
- The [yue_eucpp](../yue_eucpp) project 
- The [Moiseenko](https://gist.github.com/eupp/a78e9fc086834106e98d50e1e7bdea24) project
