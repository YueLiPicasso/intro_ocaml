# Reifiers Written in the Monad Pattern

[Technical note](./test/README.md)

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

## Credit

This is based on the [yue_eucpp](../yue_eucpp) project.
