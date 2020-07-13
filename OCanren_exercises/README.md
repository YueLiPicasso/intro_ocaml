# On Learning OCanren 

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
reproduce (or even creatively modify) the examples.  


## The OCanren exercises

The helpful expectation is not to understand OCanren in a strict axiomatic
manner, but rather like following a spiral: you first know what the
components are, and what is their layout, and then go deeper into the
components round by round, and for each round aiming for a half-clear
(not crystal-clear) understanding. Day-by-day, you would be  able to
see the intricacies more and understand what previously puzzled you. 


The [tree](tree) sample served as my frst exposure to OCanren programming.
I dashed ahead through it to have a feel of the various language constructs,
and I had to accept the fact that sometimes I was just
typing the code without knowing for sure what they do. The impression was
that there are some perculiariies related to how one should define a custom
type, and there are boilerplate pieces for injecting and projecting data which
sandwitches the actual relational programming in between. 


OCanren inherits much terminology from miniKanren,
such as reification (replacing logical variables by terms of the host language),
goal (a function which takes one substitution and returns a stream of
substitutions) etc. A major difference from miniKanren is that OCanren adopts
a delicate type hierarchy for static typing purposes, and this hierarchy is
best learnt using the [sorting](sorting) sample, accompanied by reading the
simplest OCanren standard library interface files [LNat](https://github.com/JetBrains-Research/OCanren/blob/master/src/std/LNat.mli) and [LList](https://github.com/JetBrains-Research/OCanren/blob/master/src/std/LNat.mli), where the
letter _L_ stands for _Logic_. This was my second exercise.


Next comes the [Jeep Problem](JeepProblem). Being itself mathematically complicated, a relational
encoding of the problem, however, is no more than stating the most immediate
and straightforward facts. OCanren involves a syntactic extension to OCaml
using the advanced camlp5 utility. This extension is signalled by the
`ocanren { }` construct. Exploration of this was in the context of the Jeep
problem. Also at this point a better understanding of the [LPair](https://github.com/JetBrains-Research/OCanren/blob/master/src/std/LPair.mli) library could
be gained.

