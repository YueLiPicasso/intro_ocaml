# Type exercises

I tried to define alternative types that seem simpler than the reference. However,
type errors occured when I compiled.

```ocaml
(* Types of moves *)
@type 'nat move =
  Forward  of 'nat
| Backward of 'nat
| Unload   of 'nat
| Fill     of 'nat
with show, gmap


The problem is that
in the following OCanren snippet:

`fresh d, .., in ... Forward d ...`

if using my simple definition of 