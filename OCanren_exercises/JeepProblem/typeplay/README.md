
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
