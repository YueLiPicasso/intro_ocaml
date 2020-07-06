
# Type exercises

I tried to define alternative types that seem simpler than the reference.
However, type errors occured when I compiled. Here is a description of the
problem.

The type of a basic move of the jeep is defind by the reference as:

```ocaml
(* Types of moves *)
@type 'nat move =
  Forward  of 'nat
| Backward of 'nat
| Unload   of 'nat
| Fill     of 'nat
with show, gmap
```

I thought that the type parameter `'nat` is unnecessary,
so I defined it instead as (see the source file [jeep.ml](jeep.ml)
for additional comments):

```ocaml
@type unitt = int with show;;
@type pos = unitt with show;;
@type fuel = unitt with show;;
@type move =
     Forward of pos
   | Backward of pos
   | Unload of fuel
   | Fill of fuel
 with show;;
```

The problem is from the following OCanren [snippet](https://github.com/YueLiPicasso/intro_ocaml/blob/bc738c6cf1744d764ee2c4d7c4a1c4fcc2072e0d/OCanren_exercises/JeepProblem/typeplay/jeep.ml#L204):

```ocaml
fresh d, pos', fuel' in
          move == Forward d
	                  ^
```

Referring to the `d` (over ^), the type checker reports that:
> This expression has type ('a, 'b) Logic.injected,
       but an expression was expected of type Typeplay.pos = int


## Solution

There are both something right and something wrong with:

```ocaml
let forward x = !! (Forward x)
and backward x =  !! (Backward x)
and unload x = !! (Unload x)
and fill x = !! (Fill x);;
```

The right thing is that I was trying to define injection primitives for
making injected user-type data for use in the logic domain.  The wrong thing
 is that, given that the value constructors of the type constructor
`move` have arguments, I used the `(!!)` operator which does not work as
intended with this kind of types.

For correction, I shall redefine `move` as a parametric-polymorphic
type, and then use a distribution function `distrib` together with `inj` to
inject its parameterized value constructors. More details follow.

Continuing to use the type abbreviations `pos` and `fuel`,
I shall redefine `move` as:

```ocaml
@type ('a, 'b) move =
       Forward of 'a        (* The type 'pos'  is intended for 'a *)
     | Backward of 'a
     | Unload of 'b         (* The type 'fuel' is intended for 'b *)
     | Fill of 'b
 with show, gmap;;
```

A `distrib` function comes from application of one of the `Fmap` functors.
For example, for a type constructor with two type parameters,
we shall use the `Fmap2` functor, which provides, among others, a function
`distrib` that moves the injection from the type parameters
 level to the outer type constructor level
 (see also [here](https://github.com/JetBrains-Research/OCanren/blob/26ac06ce87eaa1f1e598190b1b6daeac2bee7eac/src/core/Logic.mli#L143)): 

```ocaml
module Fmap2 (T : T2) :
 sig
   val distrib :
   (('a,'c) injected, ('b,'d) injected) T.t ->
   (('a, 'b) T.t, ('c, 'd) T.t) injected

   (* etc. *)
 end
```
 where the module type `T2` exports an abstract two-parameter type constructor
  `('a, 'b) t` and a mapping function `fmap` as follows:
  

```ocaml
module type T2 =
 sig
   type ('a, 'b) t
   val fmap : ('a1 -> 'a2) -> ('b1 -> 'b2) -> ('a1, 'b1) t -> ('a2, 'b2) t
 end
```

It actually is a necessity to make `move` polymorphic.  Provided that value
constructors (like `Forward`) of `move` have arguments, and we need to
transport the arguments back-and-forth between the logic domain
and the functional domain, and that values in these domains have distinct types,
 we must let  value constructors of `move` to have polymorphic  argument type.