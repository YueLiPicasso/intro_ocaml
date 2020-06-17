(* Tower of Hanoi *)

(* There are three rods named A, B and C *)
type rod = A | B | C

let string_of_rod = function
  | A -> "A"
  | B -> "B"
  | C -> "C"

(* encoding the move of a single disk 
   from one rod to another *)
module Move : sig
  type t = private rod * rod
  val move : rod -> rod -> t
  val to_rods : t -> rod * rod
  val string_of_move : t -> string
  val print_move : t -> unit
  val print_move_nl : t -> unit
end = struct
  type t = rod * rod
  let move x y = assert (x <> y);(x,y)
  let to_rods m = m
  let string_of_move = function
    | (x,y) -> Printf.sprintf "(%s, %s)"  (string_of_rod x)  (string_of_rod y)
  let print_move m = print_string (string_of_move m)
  let print_move_nl m = print_move m; print_newline ()
end

type moves = (Move.t) list

let print_moves : moves -> unit =
  List.iter Move.print_move_nl 
  
let free_rod : Move.t -> rod = fun m ->
  match (Move.to_rods m) with
    (A, B) | (B, A) -> C
  | (A, C) | (C, A) -> B
  | _ -> A

(*
   hanoi n x y gives the list of moves to move n disks 
   from rod x to rod y
*)
let rec hanoi : int -> rod -> rod -> moves =
  fun n x y ->
  if n = 1 then [Move.move x y]
  else if n > 1 then
    let z = free_rod (Move.move x y) in
    List.flatten [hanoi (n - 1) x z;hanoi 1 x y;hanoi (n - 1) z y]
  else raise (Invalid_argument "hanoi")
      

let _ = Printf.printf "Hello World !\nRods are %s, %s and %s\n"
    (string_of_rod A)
    (string_of_rod B) 
    (string_of_rod C)

let _ = print_string
    begin
      try Move.string_of_move (Move.move A A) ^ "\n" with
        Assert_failure _ -> "Immaterial move\n"
    end

let _ = print_string
    begin
      try Move.string_of_move (Move.move A C) ^ "\n" with
        Assert_failure _ -> "Immaterial move\n"
    end


let _ =  try print_moves (hanoi 1 B B) with
    Assert_failure _ -> print_string "Immaterial move\n"

(* Check the solution on:
   https://www.mindgames.com/game/Tower+of+Hanoi
*)
                          
let _ =  try print_moves (hanoi 6 A C) with
    Assert_failure _ -> print_string "Immaterial move\n"
