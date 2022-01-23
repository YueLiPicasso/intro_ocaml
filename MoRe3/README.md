# Reifiers Written in the Monad Pattern

This project succeeds the projects listed in the [Credits](#credits) section, bringing the understanding and implementation of monadic reification to a new stage. We are now able to answer the two major questions pursued all along the series of projects:
* Resolving the looping problem of monadic reification over recursive algebraic data types, with the most reliable, simple and readable code.
* Evaluating monadic reification as a potential technique to eliminate the need for [a predefined set of functors](https://github.com/JetBrains-Research/OCanren/blob/8ce216180e2abe37b8a1f60cf6bf9187c63fc81c/src/core/Logic.ml#L135) in the implementation of OCanren. 

The first question is the looping problem. Here looping is not because we are working on any infinite/cyclic data, but because of the call-by-value strategy of OCaml, coupled with a naive monadic style. Since we need monad (and in particular the abstraction provided thereby), our only hope is to engineer wrt. to call-by-value. Each predecessor project makes a step forward in this regard. For instance, to begin with, [Moiseenko](https://gist.github.com/eupp/a78e9fc086834106e98d50e1e7bdea24) uses a `compose` operator to stop looping for list reifiers, but this does not work for Peano numbers, as discovered by [yue_eucpp](../yue_eucpp), which is a careful review of Moiseenko's work, spliting the monolithic source file into the combination of a core, a library and a test file. Then, [monadic_reify](../monadic_reify) proposes the use of OCaml's Stdlib.Lazy as a reliable loop killer. Following that,  in [MoRe](../MoRe)  the `compose` operator is completely removed, and there are two monad binders, one for recursive and the other for non-recursive reifiers. Next, [MoRe2](../MoRe2) tries to merge the two binders into one with the help of a sum type, but the solution is critisized by [the reviewer](https://github.com/Kakadu) as being hard to read. With this in mind, and also with some experience on streams and lazy lists, finally we have the current solution: systematically define reifiers using thunks `fun () -> ...`, so that we have only one binder (see [lib/Core](lib/core.mli)) which is hopefully more readable and simple. 

The second question concerns predefined functors: can that be eliminated by monadic reifiers?. The short but firm answer is "no".  Along the line of projects we find that there are actually two kinds of reifiers: lazy and eager, indexed by the number of type parameters and the map function. (Note that "lazy" now refers to lazily evaluated infinite data, but we also used the lazy/thunk technique to solve the looping problem --- these two mentions of "lazy" are independent from each other.) Recursion is not a concern here because a recursive reifier can always be built from a non-recursive one. The [type/Common](type/common.mli) module gives the shape of the predefined functor set in the context of monadic reification.  As applications of the predefined set, we have two modules that share the same structure modulo the number of type parameters and the map function: [type/List](type/list.mli) vs. [type/Option](type/option.mli), and we additionally have [type/Either](type/either.mli). Note that they reuse the generic reifiers provided by the predefined functor set in exactly the same way as the non-monadic OCanren reifier implementation. 

Now we are well positioned to draw a conclusion on monadic reification:
- It provides good abstraction (hiding `Env.t`) and but requires understanding of monads, and also some lazy evaluation techniques to avoid looping.
- In terms of avoiding a predefined functor set, it does not help.

Some tips:
- A good starting point for understanding monadic reifiers is the basic version as in Moiseenko or yue_eucpp --- just remember the definitions and follow through the tests.
- Once the naive monadic refiers are understood, the next step is to see how to use thunk or Stdlib.Lazy to avoid looping. Our latest solution in this regard is to wrap all reifiers in a thunk (which is quite conventional and standard) and redefine the monadic binder. At last, study (say) the source of Stdlib.Seq and a couple of tutorials and see how to reify infinite lazy data.
- Oh, impressive ! The symmetries along the project evolution line, among the functors in the predefined set,  and across different types !!!


 ## Credits
 
 Predecessor projects. Top -> bottom, lastest -> oldest. 
 
- [MoRe2](../MoRe2)
- [MoRe](../MoRe)
- [monadic_reify](../monadic_reify)
- [yue_eucpp](../yue_eucpp) 
- [Moiseenko](https://gist.github.com/eupp/a78e9fc086834106e98d50e1e7bdea24)
