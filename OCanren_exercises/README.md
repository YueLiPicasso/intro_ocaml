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
the underlying formal syntax and semantics.


## The OCanren exercises

Before I started to learn OCaml, I spent about two and half
months learning about the implementation of miniKanren in Scheme. So after
five months of preparation, half on Scheme/miniKanren, and half on Ocaml, I
started  

