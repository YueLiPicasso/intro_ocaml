

open OCanren;;

@type 'a logic' = 'a logic with show;;

module Me = struct
  @type t = Me with show;;
  @type ground = t with show;;
  @type logic = t logic' with show;;
  type groundi = (ground, logic) injected;;
end;;

let me : unit -> Me.groundi = fun () -> !!(Me.Me);;

let male x = ocanren { x == Me };;

(** [foo x y] stands for: [x] is a son of [y]'s mom *)
let foo x y = ocanren { x == y & male x };;

(** [bar x y] stands for: [x] is not [y]'s brother *)
let bar x y = ocanren { x == y };;

let _ = 
  List.iter (fun x -> print_string @@ (GT.show(Me.ground) x ^ "\n"))@@
  RStream.take @@
  run q (fun who -> ocanren { foo who Me & bar who Me}) project;;
