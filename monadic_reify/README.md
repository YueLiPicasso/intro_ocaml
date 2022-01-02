# Reifiers Written in the Monad Pattern

We use lazy evaluation to solve the looping problem of certain recursive reifiers. This problem was discovered in the [yue_eucpp](../yue_eucpp) project. `Reifier.compose` was also used to tackle looping but in the case of our interest it does not work. The details are given in the technical note [Using Multiple Logical Variables](notes/multivar.md)
- [Solving the Problem of Non-terminating Monadic Reifiers for Certain Recursive Types](notes/lazy_reify.md). We guess it is possible to get rid of  `Reifier.compose` and use just lazy evaluation to solve the looping problem entirely. This idea is discussed in the note [Understanding the Monadic Reifiers](notes/type_annot.md).

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

## Technical Notes

- [Type Analysis of the Body of `Core.run`](notes/run_type.md)
- [Understanding the Monadic Reifiers](notes/type_annot.md)
- [Type Safety Case study](notes/type_safety.md)
- [Using Multiple Logical Variables](notes/multivar.md)
- [Solving the Problem of Non-terminating Monadic Reifiers for Certain Recursive Types](notes/lazy_reify.md)



## Credits

- The [yue_eucpp](../yue_eucpp) project 
- The [Moiseenko](https://gist.github.com/eupp/a78e9fc086834106e98d50e1e7bdea24) project
