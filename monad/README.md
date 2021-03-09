# Comparing Monads in Haskell and in OCaml 

_25 Feb 2021_ 

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
 
<hr>

## Follow-up Comments 

Below are some follow-up comments on the blog.

### The monadic `return` is  overloaded, or (higher-kinded) parametric polymorphic ?

_03 March 2021; edited on 09 March 2021_ 
 
 In Section 1 of the blog, I wrote that the monadic `return` function
 is higher-kinded polymorphic. I later noticed that the authors of the
 _Lightweight Higher-kinded Polymorphism_ paper do not consider `return` in 
 Haskell as being (higher-kinded) parametric polymorphic; instead, they 
 call it overloaded. Here I clarify the concepts. 
 
 
 Overloading and parametric polymorphism are two distinct 
 concepts: for a parametric polymorphic function its different type-specific
 instances share exactly the same function definition, e.g., reversal of
 a list is irrelevant to the list member type and the function definition
 is the same for lists of characters, strings, integers, booleans or floats 
 etc.; for an overloaded function its different type-specific instances 
 definitely have different function definitions, e.g., the definition 
 of `return` for a Maybe Monad is a "Maybe" wrapper, while for a List Monad
 the definition of `return` becomes a singleton-list wrapper. In other words, 
 an overloaded Haskell function and a parametric polymorphic function may have
 type signatures that look similar: in both cases there are type variabes 
 that can be instantiated to obtain signatures of  type-specific instances 
 of the function; however, a parametric polymorphic function has only one 
 concrete definition or embodiment, but an overloaded function has as many 
 concrete definitions or embodiments as the types for which the function 
 is overloaded. 
 
 Assigning a type to an overloaded function by means of a constructor class
 (such as the `Monad` class in Haskell) results in occurrences of 
 type variables in the signatures of certain functions; but occurrence of 
 instantiable type variables in the type signature does not imply that a 
 function so typed is necessarily parametric polymorphic.  
 
 To be more precise, the monadic `return` function is a (higher-kinded) 
 _ad-hoc_ polymorphic function (overloading = ad-hoc polymorphism), but 
 not a _parametric_ polymorphic function.
 It is therefore correct to say (albeit less precisely) that it is a 
 higher-kinded polymorphic function (as I say in Section 1). 
 
 
### The subtlety of checking type equality in OCaml
 
_09 March 2021_ 
 
Following a comparison of the `when` function in Haskell with that in
OCaml, the authors comment that the extra heaviness of writing `when` in OCaml
comes directly from the lack of overloading in the language; but more deeply (or more
generally) the reason is the lack of higher-kinded polymorphism. 

I understand that `return` is higher-kinded ad-hoc polymorphic. To have higher-kinded 
ad-hoc polymorphism in OCaml,  which is a special form of higher-kinded polymorphism, 
we must provide what is required by _any_ form of higher-kinded polymorphism, be it ad-hoc 
or parametric; we must allow type variables of higher-kinds, 
or some other way of abstracting over higher-kinded type constructors. 

Then the authors move on to discuss the _alias problem_. They begin with the 
argument that checking type equality is easier in Haskell than in OCaml. I have some 
comments here. 

I understand that in Haskell there is no way to hide a type equation (aka. a type synonym); only 
data constructors can be hidden and the programmer can choose to hide all or only
some of the data constructors of an algebraic data type. But in OCaml, a
type equation can be hidden; data constructors are exported in an all-or-nothing
manner. In the above sense (and compared with Haskell) OCaml offers more flexibility 
for hiding type equations, but less flexibility for hiding data constructors. 

The authors say that an OCaml module signature (aka. module type) _may_ temporarily
abstract a type in the case of applying a module functor. This is true and this is indeed 
a subtle technical point of OCaml modules. OCaml functors require that their argument 
modules must match the module types specified in the functor definitions. Explicit argument 
module types in a functor definition tells what the functor assumes about 
its argument modules, but these module types as functor argument specifications have no 
effect on what the type checker can see about the argument modules.  A functor can assume 
about an argument module no more than what is given in the argument signature but outside
the functor, the type checker could see the full picture of the argument module which is 
given by the module's own signature. 
 
<hr>
<em>This blog post is also on <a href="https://github.com/YueLiPicasso/intro_ocaml/tree/master/monad">GitHub</a> with some extra code examples.</em>
