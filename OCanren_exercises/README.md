# How I Learnt OCanren 

OCanren is implementation of miiniKanren in OCaml. Its obvious prerequisites
are miniKanren (Scheme-based) and OCaml. A good knowledge of Prolog and
SLD-resolution is also very helpful for understanding the high-level behavior
of OCanren (and miniKanren as well). 


## Learning OCaml

The OCaml tutorial (Part I of the [reference manual](http://caml.inria.fr/pub/docs/manual-ocaml/)) covers the features of the core language, such as types,
let-bindings, pattern matching, exception handling, modules and classes etc. I
also learnt the first few sections of the [language extensions](http://caml.inria.fr/pub/docs/manual-ocaml/extn.html) chapter. When working with the tutorials,
I found it very helpful to refer frequently to [The OCaml language](http://caml.inria.fr/pub/docs/manual-ocaml/language.html) chapter, which presents systematically
the formal syntax and (informal) semantics of all features of the core
language. Almost for each feature that I learnt from Part I, I would go to this chapter to read about
the underlying formal syntax and semantics. Jumping between examples and grammar in this way inevitably slows down your speed, but it gives you a firmer
grasp of the language which otherwise could not be gained when you merely
reproduce (or even modifying) the examples.  


## The OCanren exercises


OCanren inherits much terminology from miniKanren,
such as reification (replacing logical variables by terms of the host language),
goal (a function which takes one substitution and returns a stream of
substitutions) etc. A major difference from miniKanren is that OCanren adopts
a delicate type hierarchy for static typing purposes, and this hierarchy is
best learnt using the [sorting](sorting) sample, accompanied by reading the
simplest OCanren standard library interface files [LNat](https://github.com/JetBrains-Research/OCanren/blob/master/src/std/LNat.mli) and [LList](https://github.com/JetBrains-Research/OCanren/blob/master/src/std/LNat.mli), where the
letter _L_ stands for _logic_. 

