(1 < 2) = false;;

let rec sort lst =
  match lst with
    [] -> []
  | head :: tail -> insert head (sort tail)
and insert elt lst =
  match lst with
    [] -> [elt]
  | head :: tail -> if elt <= head then elt :: lst
    else head :: insert elt tail
;;

let l = ["Life";"is";"a";"tale";"told";"etc."];;

sort l;;

sort [3.14 ; 2.718];;

{|"\\"|} = "\"\\\\\"";;

let l2 = "My" :: l;;

l;;

let deriv f dx = function x -> (f (x +. dx) -. f x) /. dx;;

let sin' = deriv sin 1e-6;;

let pi = 4.0 *. atan 1.0;;

sin' pi;;

let compose f g = function x -> f (g x);;

let square x = x *. x;;

let cos2 = compose square cos;;

List.map (function n -> n * 2 + 1) [0;1;2;3;4];;

List.map;;

let rec map f l =
  match l with
    [] -> []
  | hd :: tl -> f hd :: map f tl;;

type ratio = {num : int; denom : int};;

let add_ratio r1 r2 =
  {num = r1.num * r2.denom + r2.num * r1.denom;
  denom = r1.denom * r2.denom};;

add_ratio {num = 1; denom =3} {num =2; denom=5};;

let integer_part r =
  match r with
    {num = num;denom = denom} -> num / denom;;

let integer_part {num=num; denom=denom} = num/denom;;

let get_denom {denom = denom} = denom;;

let get_num {num=num;_} = num;;

let integer_part {num; denom} = num / denom;;

let ratio num denom = {num ; denom};;

let integer_product integer ratio
  = {ratio with num = integer * ratio.num};;

let myratio = {num=2;denom=3};;

let myratio' = myratio;;

let myratio'' = integer_product 5 myratio;;

myratio = myratio'';;

myratio' = myratio;;

let myratio''' = ratio 2 3;;

myratio' = myratio''';;

type number = Int of int | Float of float | Error;;

type sign = Positive | Negative;;

Int 5;;

Float 5.0;;

let sign_int n =
  if n >= 0
  then Positive
  else Negative;;

let add_num n1 n2 =
  match (n1, n2) with
    (Int i1, Int i2) -> Int (i1 + i2)
  | (Float f1, Float f2) -> Float (f1 +. f2)
  | (Error, _) -> Error
  | (_, Error) -> Error
  | (Float f, Int i) -> Float (f +. float i)
  | (Int i , Float f) -> Float (f +. float i)
;;

add_num (Int 123) (Float 123.0);;

type 'a option = Some of 'a | None;;

let safe_square_root x =
  if x > 0. then Some (sqrt x) else None;;

type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;

let rec member x btree =
  match btree with
    Empty -> false
  | Node (y, left, right) ->
    if x = y then true else
    if x < y then member x left else member x right;;

let rec insert x btree =
  match btree with
    Empty -> Node (x, Empty, Empty)
  | Node (y, left, right) ->
    if x <= y then Node (y, insert x left, right)
    else Node (y, left, insert x right);;

type family = {dad : string; mom : string ; kid : string};;

let get_dad {dad} = dad;;

let family dad mom kid = {dad ; mom ; kid};;

family "Min" "Shan" "Yue";;

let add_vect v1 v2 =
  let len = min (Array.length v1) (Array.length v2) in
  let res = Array.make len 0.0 in
  for i = 0 to len -1 do
    res.(i) <- v1.(i) +. v2.(i)
  done;
  res;;

add_vect [|1.0 ; 2.0|] [| 3.0 ; 4.0|];;

type mutable_pointt = {mutable x : float ; mutable y : float};;

let translate p dx dy =
  p.x <- p.x +. dx;
  p.y <- p.y +. dy;;

let mypoint = {x = 0. ; y = 0.};;

translate mypoint 1. 2.;;

mypoint;;

type 'a ref = {mutable contents : 'a};;

let ( ! ) r = r.contents;;

let ( := ) r newval = r.contents <- newval;;

let myref = {contents = 'y'};;

myref;;

! myref;;

myref := 'z';;

! myref;;

let ref a = {contents = a};;

ref 'x';;

let current_rand = ref 0;;

let random () =
  current_rand := !current_rand * 25713 + 1345;
  !current_rand;;

random ();;
random ();;
random ();;
!current_rand;;

let insertion_sort a =
  for i = 1 to Array.length a - 1 do
    let tmp = a.(i) in
    let j = ref i in
    while !j > 0 && tmp < a.(!j - 1) do
      a.(!j) <- a.(!j - 1);
      j := !j - 1
    done;
    a.(!j) <- tmp
  done;;

let myarray = [|4;3;2;1|];;

insertion_sort myarray;;

myarray;;

exception Empty_list;;

let head l =
  match l with
    [] -> raise Empty_list
  | hd :: _ -> hd;;
          
head [1;2];;
head [];;

List.assoc 1 [(0, "zero");(1, "one")];;

List.assoc 2 [(0, "zero");(1, "one")];;

let bname b =
  try List.assoc b [(0, "zero");(1, "one")]
  with Not_found -> "no binary";;

bname 1;;

bname 0;;

bname (-1);;

let rec first_named_value values names =
  try
    List.assoc (head values) names
  with
  | Empty_list -> "no named value"
  | Not_found -> first_named_value (List.tl values) names;;

first_named_value [0 ; 10] [(0, "zero");(10, "ten")];;
                                                           
first_named_value [2 ; 10] [(0, "zero");(10, "ten")];;


let lazy_two = lazy ( print_endline "eval lazy two"; 1 + 1 );;

Lazy.force lazy_two;;

lazy_two;;

let lazy_l = lazy ([1;2]@[3;4]);;

let lazy l = lazy_l;;

let lf = lazy ( false ) ;;

Lazy.force lf;;

type expression =
    Const of float
  | Var of string
  | Sum of expression * expression   (* e1 + e2 *)
  | Diff of expression * expression  (* e1 - e2 *)
  | Prod of expression * expression  (* e1 * e2 *)
  | Quot of expression * expression  (* e1 / e2 *)
;;

exception Unbound_variable of string;;

let rec eval env exp =
  match exp with
    Const c -> c
  | Var v ->
    (try List.assoc v env with
       Not_found -> raise (Unbound_variable v))
  | Sum(f,g) -> eval env f +. eval env g
  | Diff(f,g) -> eval env f -. eval env g
  | Prod(f,g) -> eval env f *. eval env g
  | Quot(f,g) -> eval env f /. eval env g;;

eval [("x", 1.0);("y", 3.14)] (Prod(Sum(Var "x", Const 2.0), Var "y"));;


let rec deriv exp dv =
  match exp with
    Const _ -> Const 0.
  | Var v -> if v = dv then Const 1. else Const 0.
  | Sum(f,g) -> Sum(deriv f dv, deriv g dv)
  | Diff(f,g) -> Diff(deriv f dv, deriv g dv)
  | Prod(f,g) -> Sum(Prod(deriv f dv, g),Prod(f, deriv g dv))
  | Quot(f,g) -> Quot(Diff(Prod(deriv f dv, g),Prod(f, deriv g dv)),
                      Prod(g,g))
;;

deriv (Quot(Const 1. , Var "x")) "x";;

let open_paren prec op_prec =
  if prec > op_prec
  then print_string "(";;

let close_paren prec op_prec =
  if prec > op_prec
  then print_string ")";;

(* prec is the immediate context of exp.
   There are only four possible contexts: 
   Sum, Diff, Prod and Quot, where any one can 
   be the immediate context of any one. For example,
   when printing Prod(Sum(_,_), _), Prod is the context of Sum,
   and we need parentheses around Sum; however, when printing
   Sum(Prod(_,_),_), we do not need parentheses around Prod. 

   When printing one of the context Sum, Diff, etc., two things
   shall be considered: shall we wrap this expression itself? and
   shall we wrap any of its immediate sub-expression? *)

let rec print prec exp =
  match exp with
    Const c -> print_float c
  | Var v -> print_string v
  | Sum(f,g) ->
    open_paren prec 0;
    print 0 f; print_string " + "; print 0 g;
    close_paren prec 0
  | Diff(f,g) ->
    open_paren prec 0;
    print 0 f; print_string " - "; print 1 g;
    close_paren prec 0
  | Prod(f,g) ->
    open_paren prec 2;
    print 2 f; print_string " * "; print 2 g;
    close_paren prec 2
  |Quot(f,g) ->
     open_paren prec 2;
    print 2 f ; print_string " / "; print 3 g;
    close_paren prec 2
;;

let print_expr exp = print 0 exp;;    


let e = Quot(
    Quot(Prod(Const 1. , Const 1.),
         Prod(Var "x", Var "y")),
    Quot(Sum(Const 5.78 , Var "x"),
         Diff(Const 7.88, Var "x")));;

print_expr e; print_newline ();;

print_expr (deriv e "x"); print_newline ();;

Printf.printf "%i + %i , %F + %F, %S\n" 3 2 4.5 1. "hello";;

let pp_int ppf n = Printf.fprintf ppf "%d" n;;

Printf.printf "Printed by a custom printer:\n %a" pp_int 42;;

let pp_option printer ppf = function
  | None -> Printf.fprintf ppf "None"
  | Some v -> Printf.fprintf ppf "Some(%a)" printer v;;

Printf.fprintf stdout
  "Current setting %a.\n There is %a\n"
  (pp_option pp_int) (Some 3)
  (pp_option pp_int) None;;

let v1 = ref 1;;
(!v1);;
v1 := 2;;

Sys.int_size;;

fst (1 , 2);;
snd (1 , 2);;

let var x = ref x;;
let is_var r =
  match r with
  |{ contents = _ } -> true;;

let empty_s = [];;

let ext_s x v s = (x , v) :: s;;

let rec walk v s =
  match List.assq_opt v s with
  | None -> v
  | Some u -> walk u s
;;

type data =
    Var of int ref
  | Sym of string
  | Num of int
  | Lst of data list;;

[Var (ref 1); Sym "apple"; Num 1; Lst [Var (ref 2); Lst [] ]];;

(* ppf is the output channel *)
(* This is a more flexible printer that can be combined with
   other printers *)
let pp_expr ppf expr =
  let open_paren prec op_prec output = (* output is the output channel*)
    if prec > op_prec then Printf.fprintf output "%s" "(" in
  let close_paren prec op_prec output =
    if prec > op_prec then Printf.fprintf output "%s" ")" in
  let rec print prec ppf expr =
    match expr with
    | Const c -> Printf.fprintf ppf "%F" c
    | Var v -> Printf.fprintf ppf "%s" v
    | Sum(f,g) ->
      open_paren prec 0 ppf;
      Printf.fprintf ppf "%a + %a" (print 0) f (print 0) g;
      close_paren prec 0 ppf
    | Diff(f,g) ->
      open_paren prec 0 ppf;
      Printf.fprintf ppf "%a - %a" (print 0) f (print 1) g;
      close_paren prec 0 ppf
    | Prod(f,g) ->
      open_paren prec 2 ppf;
      Printf.fprintf ppf "%a * %a" (print 2) f (print 2) g;
      close_paren prec 2 ppf
    | Quot(f,g)->
      open_paren prec 2 ppf;
      Printf.fprintf ppf "%a / %a" (print 2) f (print 3) g;
      close_paren prec 2 ppf
  in print 0 ppf expr;;

e;;

pp_expr stdout e; print_newline ();;

pp_expr stdout (deriv e "x"); print_newline();;

let str : _ format = "%i is int, %F is float and %s is string";;

Printf.printf str 3 3. "hi";;
