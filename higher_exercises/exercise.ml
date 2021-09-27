(* the "higher" utility *)

type ('p, 'f) app

module type Newtype1 = sig
  type 'a s
  type t
  external inj : 'a s -> ('a, t) app 
    = "%identity"
  external prj : ('a, t) app -> 'a s
    = "%identity"
end 

module Common =
struct
  type t
  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

module Newtype1 (T : sig type 'a t end) : Newtype1 with type 'a s = 'a T.t =
struct
  type 'a s = 'a T.t
  include Common
end

  (* my workouts *)

module List = Newtype1(struct type 'a t = 'a list end)

let a = ([1] : int List.s)
let b = (a : int list)

(* nested / non-regular datatype *)                                 

type 'a nest = NilN | ConsN of 'a * ('a * 'a) nest                                     

let (a : int nest) = NilN;;
let (a : int nest) = ConsN (1, NilN);;
let (a : int nest) = ConsN (1, ConsN ((1,2), (NilN : ((int*int)*(int*int)) nest)));;

type 'a bush = NilB | ConsB of 'a * ('a bush) bush

let (a : int bush) = NilB;;
let (b : int bush) = ConsB (1, (NilB : (int bush) bush));;
let (c : int bush) = ConsB (1, (ConsB (b, (NilB: ((int bush) bush) bush))));;
let (d : int bush) = ConsB (1, (ConsB(c, NilB)));; (* [1, [1, [1]]] *)

(* essence of fold *)

type mystr = NilS | ConsS of char * mystr

let rec foldS nilS consS str = match str with
    NilS -> nilS
  | ConsS (c, s) -> consS c (foldS nilS consS s)

let sumS l = foldS 0 (fun c i -> int_of_char c + i) l

let a = sumS (ConsS ('a', ConsS ('a', NilS))) 

