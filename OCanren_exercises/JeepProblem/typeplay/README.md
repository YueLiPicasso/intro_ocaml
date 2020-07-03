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
so I defined it instead as (see the source file typeplay.ml
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

The problem is that
in the following OCanren snippet:

`fresh d, .., in ... Forward d ...`

if using my simple definition of 