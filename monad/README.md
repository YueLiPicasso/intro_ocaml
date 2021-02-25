# Comparing Monads in Haskell and in OCaml 

_Updated on: 25 Feb 2021_ 

## 1.

In the introduction part of the paper [Lightweight Higher-kinded Polymorphism](https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf), the authors compared 
two implementations of the `when` function: one in OCaml and the other in Haskell. 

The `when` function as defined in Haskell is not simply a conditional with a 
default. More importantly, the types of the branches are monadic. 
For instance, the default branch `return ()` (read "return unit") is a 
higher-kinded polymorphic term of type `Monad m => m ()` (read: "m unit, where
the type constructor m can be any monad", or simply "monad unit"); and `return` itself has 
type `Monad m => a -> m a` (read: "a arrow m a, where the type constructor m can be any monad, and a can be any type", or simply "a arrow monad a"). 

In OCaml, the difficulty of implementing `when` lies not in the conditional 
but in the higher-kinded polymorphic `return` function, for OCaml does not 
support type variables of higher-kind: the higher-kinded type variables 
(the `m`'s) in the signatures in the above paragraph have no parallel in OCaml. 

The authors then point out that OCaml provides _module functors_ to abstract
over type constructors, and show a way to implement `when` in OCaml, subject 
to the reader's know-how about implementing [monads in OCaml](https://www.cs.cornell.edu/courses/cs3110/2020fa/textbook/adv/monads.html). 

I observe that the difference 
between monads in OCaml and monads in Haskell is the root of the 
difference between the `when` functions. 

## 2. 

First let's see the similarity between monads in the two languages:
why they are essentially the same so that we can call them both "monads"? 


In OCaml, a monad is a module featuring an abstract unary type constructor with
an arbitrary name, say `t`, and two monadic operators on `t` which are `return` and `>>=` (read: "bind"). What is important about `t` is not its name, but
its number of type parameters: 1, and the two operators on it: "return" 
and "bind". This is exactly what is captured by the `Monad` constructor class
in Haskell.  

Now we compare the difference.  

In Haskell, to declare a type constructor 
as a monad, we simply need  to declare it as an instance of
the `Monad` class, and  provide an implementation of the monadic operators
and perhaps check that the monadic laws hold. 

While, in OCaml we begin with defining a module type `Monad` consisting of 
an abstract type constructor and the signatures of the two monadic operators;
the former (i.e., the abstract type constructor) corresponds in spirit to the constructor class parameter in Haskell, and the latter (i.e. the signatures
of the monadic operators) corresponds directly to the operator type 
declarations in the Haskell constructor class. For each concrete type 
constructor which we want to declare as a monad, we must define a module 
in which we alias the type constructor and implement the 
monadic operators in such a way that the  module can be masked by the 
`Monad` module type.    

## 3.

Such difference in monad representation entails that  there is 
one monadic `return` function in Haskell which receives the type
`Monad m => a -> m a` but there are infinitely many monadic
`return` functions in OCaml each receiving a type where there is no polymorphism on
type constructors. This resembles the 
contradiction between Church typing and Curry typing;  for instance, 
there is only one identity function by Curry typing, which receives a parametric polymorphic type, but there are infinitely many identity functions by Church
 typing each receiving a concrete type.
 

It is because we have infinitely many monadic `return` functions in OCaml 
, each from a  module that implements a specific monad, that we
have infinitely many `when` functions in OCaml, each comes from an application
of the `When` module functor to a module that implements a specific monad and 
provides a specialized `return` function. 


## 4.

The interesting conclusion is that if a Haskell function has a higher-kinded polymorphic
type, and we want to implement the function in OCaml, it is inevitable that we end up with
 a family of type-specific versions of the function defined by means of modules and module functors.
