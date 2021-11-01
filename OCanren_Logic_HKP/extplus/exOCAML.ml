external plus : int -> int -> int -> int -> int -> int -> int
  = "plus_bytecode" "plus_native";;

(* inspect immedaite values *)
external inspect : 'a -> 'a = "inspect";;

(* inspect blocks *)
external inspect_b : 'a -> 'a = "inspect_block";;

external explore : string -> string = "explore_string";;

(*
type 'a tsil = Lin | Iln | Snoc of 'a * 'a tsil;;

type 'a lab = A | B of 'a | C | D of int;;

print_int (plus 1 2 3 4 5 6) ; print_newline ();;

inspect max_int;;

inspect min_int;;

inspect 100;;

inspect 'a';;

inspect Lin;;

inspect Iln;;

inspect (Snoc(1, Lin));;

inspect A;;

inspect (B 'a');;

inspect C;;

inspect (D 100);;

let show = fun a -> print_int (inspect a); print_newline ();;

show (-1);;

show (-2);;

show (-3);;

show (-4);;

show (-5);;

show (-6);;

(* integers in OCaml is encoded by two's-complement followed by 1-bit
   left shift, and then set the LSB to 1 to indicate it is an immediate
   value. LSB is 0 if it is a pointer. *)

inspect_b [| 1;2;3;4|];;

inspect_b (10, true, ());;

type foo = {fld1 : int; mutable fld2 : int};;
type fOO' = {a : char; b : char};;

inspect_b {fld1=97; fld2=98};;
inspect_b {a = 'a'; b='b'};;

inspect_b ([|'a'; 'b'; 'c'|], (`A, `B), 2.343, {fld1=10; fld2=100});;

type bar = C0 | C1 of int | C2 of bool * bool | C3 of float * float;;

print_newline();;

inspect_b C0;;

inspect_b (C1 2);;

inspect_b @@ C2(true, false);;

inspect_b @@ C3(4.5, 6.6);;

type bar' = C0' | C1' of int | C2' of bool * bool
            | C3' of float * float * int * bool * bar * bar';;

print_newline();;

inspect_b C0';;

inspect_b (C1' 2);;

inspect_b @@ C2'(true, false);;

inspect_b @@ C3'(4.5, 6.6, 888, false, C3(1.01111, 2.8877654), C0');;

inspect_b @@ C1' (Obj.magic @@ C1' (Obj.magic (C1 100)));;

inspect_b [];;

inspect_b [1;2;3;4;5];;

type goo = G of int * bool;;

inspect_b (1 :: 2 :: 3 :: 4 :: (Obj.magic (G(5, false))));;

type goo2 = G2 of int * int;;

type goo3 = G3 of int * int;;

inspect_b (1 :: 2 :: Obj.magic(G(3, Obj.magic(G2(4,Obj.magic(G3(5,0)))))));;

inspect_b "12345678\
           12345678\
           1234567";;

explore "12345678";;

explore "1234567";;

explore "This is a string\000 !" ;;


let g1 = function  x -> function y -> x + y;;

let g2 = let x = 88 and y = 99 in fun a -> a + x + y;;

inspect_b g1;;

inspect_b g2;;

type anchor = A of unit;; 
       
let global_anchor = A();;

inspect_b global_anchor;;

 *)

type person = {
    name   : string
  ; mutable age    : int
  ; gender : bool ref
  ; mutable salary : int option};;

let make name age gender salary : person = {name; age; gender; salary}
                                         
let tom : person = make "Tom" 20 (ref true) (Some 1800);;

inspect_b tom;;

inspect_b (Obj.repr tom);;

inspect_b (Obj.field (Obj.repr tom) 0);; 

inspect_b (Obj.field (Obj.repr tom) 1);;

inspect_b (Obj.field (Obj.repr tom) 2);;

inspect_b (Obj.field (Obj.repr tom) 3);; 



