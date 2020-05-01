module StrSet = AbsSet(OrderedString);;

module AbsSet = (Set : SETFUNCTOR);;

module StringSet = Set(OrderedString);;

StringSet.(member "foo" (add "bar" (add "foo" empty)));; 


(* open Base;; *)

(* let (ints,strings) = List.unzip [(1,"one"); (2,"two"); (3,"three")];; *)

module AbsPrioQueOpt = (PrioQueueOpt : PRIOQUEUE_WITH_OPT);;

AbsPrioQueOpt.(extract_opt (insert (insert empty 5 2) 6 1));;

let open PrioQueue in
let q = Node (1, "hello", Empty, Empty) in
remove_top q;;

let open PrioQueue in
let q = Node (1, "hello", Empty, Empty) in
extract q;;

let open PrioQueueOpt in
let q = Node (1, "hello", Empty, Empty) in
remove_top_opt q;;

let open PrioQueueOpt in
let q = Node (1, "hello", Empty, Empty) in
extract_opt q;;

__LOC__;;
__MODULE__;;
__POS__;;

ignore 5;;


(* We can obtain a new module by restricting
   an existing module using a signature*)
module AbstractPrioQueue = (PrioQueue : PRIOQUEUE);;

AbstractPrioQueue.(insert empty 1 "hello");;


PrioQueue.insert PrioQueue.empty 1 "hello";;

let open PrioQueue in
insert empty 1 "hello";;

PrioQueue.(insert empty 1 "hello");;


(* PrioQueue.([empty]) is a local open, where [empty]
   is the body of that local open *)

PrioQueue.[empty] = PrioQueue.([empty]);;

PrioQueue.[|empty|] = PrioQueue.([|empty|]);;

PrioQueue.{contents = empty} = PrioQueue.({contents = empty});;

PrioQueue.[insert empty 1 "hello"];;

let at_most_one_element x = match x with
  | PrioQueue.( Empty| Node(_,_,Empty,Empty))-> true
  | _ -> false;;

(* not work:  
let open PrioQueue in
let at_most_one_elem x = match x with
  Empty | Node(_,_,Empty,Empty) -> true
   | _ -> false;; *)




let isvowel c =
  c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u';;

isvowel 'l';;

let addtoten a b = a + b = 10;;

addtoten 6 4;;

exception Invalid_arg;;

let rec factorial n =
  if n <= 0 then raise Invalid_arg
  else if n = 1 then 1
  else n * factorial (n - 1);;


let isvowel' c =
  match c with
  'a' | 'e' | 'i' | 'o' | 'u' -> true
      | _ -> false;;

let not b =
  match b with
    true -> false
  | false -> true;;

let rec pow x n =
  match n with
  | 0-> 1
  | n -> if n > 0
    then x * pow x (n - 1)
    else raise Invalid_arg;;

pow 4 (5);;


let islower c =
  match c with
  'a' .. 'z' -> true
  | 'A' .. 'Z' -> false
  | _ -> raise Invalid_arg;;

let isupper c = not @@ islower c;;


type 'a logic = Logic of 'a;;

let inj (d : 'a) : 'a logic =
  Logic d;;

inj 1;;
inj [1;2;3];;




let var (v : 'a) : ('a ref) logic =
  inj (ref v);;

var 5;;

type 'a subst = ('a * 'a) list;;

let rec walk (u : 'a logic) (s : 'a logic subst) : ('a logic) =
  Logic 1;;

[1;2;3] @ [2;3;4];;

let rec odd_elem_inner l ol =
  match l with
    a :: _ :: t -> odd_elem_inner t (a :: ol)
  | [a] -> a :: ol
  | _ -> ol;;

odd_elem_inner ['a';'b';'c';'b';'e'] [];;

let rec append3 a b c =
  match a with
    [] -> b @ c
  | h :: t -> h :: append3 t b c;;

append3 [1;2;3] [4;5;6] [7;8;9];;

let rec sort l =
  match l with
    [] -> []
  | h :: t -> let rec insert x l =
                match l with
                  [] -> [x]
                | h :: t ->
                  if x <= h
                  then x :: h :: t
                  else h :: insert x t
    in insert h (sort t);;

sort ["apple";"abel";"await"];;

sort ['e';'b';'a'];;

let _'''''' = 1;;


let compose f g = fun x -> f (g x);;

let rec power f n =
  if n <= 0 then (fun x -> x) else compose f (power f (n - 1));;

let pi = 4. *. atan 1.;;

let derivative dx f = function x -> (f (x +. dx) -. f x) /. dx;;

let sin''' = (power (derivative 1e-5) 3) sin;;

sin''' pi;;


