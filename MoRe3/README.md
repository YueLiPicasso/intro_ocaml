# Reifiers Written in the Monad Pattern

This project succeeds the projects listed in the [Credits](#credits) section, bringing the understanding and implementation of monadic reification to a new stage. We are now able to answer the two major questions pursued all along the journey:

* Resolving the looping problem of recusively defined monadic reifiers with the most reliable, simple and readable code.
* Evaluating monadic reification as a potential technique to eliminate the need for [a predefined set of functors](https://github.com/JetBrains-Research/OCanren/blob/8ce216180e2abe37b8a1f60cf6bf9187c63fc81c/src/core/Logic.ml#L135) in the implementation of OCanren. 

## Answering the looping problem

We distinguish "intrisic" vs. "data-driven" looping.  Intrinsic looping of monadic reifiers is not because we are working on any infinite/cyclic data, but because of the call-by-value strategy of OCaml, coupled with a naive monadic coding style. Data-driven looping occurs when there is _no_ intrinsic looping but computation over infinite data is not properly controlled/suspended. The looping problem of monadic reification refers to intrinsic looping, because data-driven looping is common for both monadic and non-monadic reification. Actually we successfully addressed both forms of looping simultaneously in this project, but we shall defer the discussion about data-driven looping until [the next section](#answering-the-predefined-set-problem). Since we need monad (and in particular the abstraction provided thereby), our only hope is to engineer wrt. to call-by-value, in order to prevent intrinsic looping. Each predecessor project makes a step forward. For instance, to begin with, [Moiseenko](https://gist.github.com/eupp/a78e9fc086834106e98d50e1e7bdea24) uses a `compose` operator to stop looping for list reifiers, but this does not work for Peano numbers, as discovered by [yue_eucpp](../yue_eucpp), which is a careful review and reproduction of Moiseenko's work. Then, [monadic_reify](../monadic_reify) proposes the use of OCaml's Stdlib.Lazy as a reliable loop killer. We know that a thunk `fun () -> ...` can also be used for the same effect, but Stdlib.Lazy is conceptually more straightforward, hiding the details of how exactly the computation is suspended, whilst a thunk in comparison is quite technically involved, and it is just one of many possible ways to suspend computation.  A _distinction_ is maintained between two classes of reifiers: those that use  some lazy evaluation mechanism to prevent looping, and those that do not have the looping problem and do not need any form of lazy evaluation. Correspondingly, there are two monadic binders.  Then, in [MoRe](../MoRe)  the `compose` operator is completely removed, but the lazy/eager distinction of reifiers remains, so there are still explicitly two binders. [MoRe2](../MoRe2) then superficially merges the two binders into one with the help of a sum type without eliminating the underlying lazy/eager distinction over reifiers. This amounts to a transition from the explicit two binder approach to an implicit two bind approach.  With increased familiarity with the implementation of thunk-based lazy lists, and an understanding of the technical difference between Stdlib.Lazy and thunk (besides suspending computation, Stdlib.Lazy under the hood also uses memoization to avoid recomputation of forced values), here we present the successful experiment of systematically defining reifiers using thunks, thus the lazy/eager distinction of reifiers is removed, all reifiers are lazy, and intrinsically only one binder is required (see [lib/Core](lib/core.mli)), which is hopefully more readable and simple compared with the (explicit/implicit) two binder approach.

## Answering the predefined set problem

The second question concerns predefined functors: can that be eliminated by _monadic_ reification? The short but firm answer is "no". We find that there are actually two kinds of basic reifiers which are non-recusive and can be used to define all other reifiers of interest, and they are indexed by the number of type parameters and the map function --- implyiing a predefined functor set.  The [type/Common](type/common.mli) module gives the shape of the predefined functor set in the context of monadic reification. For each functor FmapN we distinguish `FmapN(T).reify` and `FmapN(T).Lazy.reify`. The former is for reification of finite data and the latter is for lazy reification of infinite data. Note that lazy evaluation is useful in two aspects: one is to prevent the [intrinsic looping](#answering-the-looping-problem) of the monadic reifiers, and the other is to allow reification of infinite data. Note also that the generic reifiers provided by FmapN are all non-recursive, but this is _not_ a problem  because we show that a recursive reifier can always be built from a generic non-recursive one, no matter you work with finite or infinite data. As examples of using the predefined functor set, we have two modules that share the same structure modulo the number of type parameters and the map function: [type/List](type/list.mli) vs. [type/Option](type/option.mli), and we additionally have [type/Either](type/either.mli). They reuse the generic reifiers provided by the predefined functor set in exactly the same way as the non-monadic OCanren reifier implementation. 

## Conclusion and tips

Now we are well positioned to draw a conclusion on monadic reification:

- It provides good abstraction (hiding `Env.t`) and but requires understanding of monads, and also some lazy evaluation techniques to avoid intrinsic looping.
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
