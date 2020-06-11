(* Recursive values *)

let rec oz = 1 :: zo and zo = 0 :: oz in (oz, zo);;
(* 
- : int list * int list = ([1; 0; <cycle>], [0; 1; <cycle>]) 
*)

(* GT motivating examples *)

(* a (custom) type *)
type expr =
  | Const of int
  | Var of string
  | Binop of string * expr * expr;;

(* simple priority assignment *)
let prio = function
    "+" -> 1
  | "-" -> 2
  | "*" -> 3
  | "/" -> 4
  | "exp" -> 5
  | s -> raise (Invalid_argument s);;

(* operator of string *)
let op = function
    "+" -> ( + )
  | "-" -> ( - )
  | "*" -> ( * )
  | "/" -> ( / )
  | "exp" ->  fun m n ->
    let m' = float_of_int m and n' = float_of_int n in
    (int_of_float (m' ** n'))
  | s -> raise (Invalid_argument s);;

(* variable state *)

let state = function
    "x" -> 2
  | "y" -> 10
  | "z" -> 4
  | s -> raise (Invalid_argument s);;

(* polymorphic identity function*)
let id : 'a. 'a -> 'a = fun x -> x;;

(* add parentheses *)
let br = fun (s : string) -> "( " ^ s ^ " )";;

br "hello";;


module ExprPrint =
struct
  (* a type-indexed function *)
  let rec show'expr = function
    | Const n -> "Const " ^ string_of_int n
    | Var x -> "Var " ^ x
    | Binop (o, l, r) ->
      Printf.sprintf "Binop (%S, %s, %s)" o (show'expr l) (show'expr r);;
  
  (* another function indexed by type expr *)
  let pretty'expr e =
    let rec pretty_prio p = function
      | Const n -> string_of_int n
      | Var x -> x
      | Binop (o, l, r) ->
        let po = prio o in
        (if po <= p then br else id) @@
        pretty_prio po l ^ " " ^ o ^ " " ^ pretty_prio po r
    in
    pretty_prio min_int e;;
end;;


(* Extracting pattern matching from "pretty" and "show", 
   we get the traversal function gcata'expr indexed by the 
   type expr. It takes three parameters: 
   -- omega is a transformation object
   -- iota is an extra parameter that can be used or ignored; 
   it corresponds to the priority value used by pretty_prio
   -- a value of type expr
 *)

let gcata'expr omega iota = function
  | Const n -> omega # pConst iota n
  | Var x -> omega # pVar iota x
  | Binop (o, l, r) -> omega # pBinop iota o l r;;

(* we can redefine show'expr and pretty'expr using 
   the traversal function gcata'expr for type expr 
   with custom transformation objects
*)

(* using an immediate object *)
module ExprImO =
struct
  let rec show'expr e =
    let transobj =
      object
        method pConst _ n = "Const " ^ string_of_int n
        method pVar _ x = "Var " ^ x
        method pBinop _ o l r =
          Printf.sprintf "Binop (%S, %s, %s)" o (show'expr l) (show'expr r)
      end
    in gcata'expr transobj () e;;
  
  let pretty'expr e =
    let rec pretty_prio p e' =
      let transobj = object
        method pConst _ n = string_of_int n
        method pVar _ x = x
        method pBinop p o l r = let po = prio o in
          (if po <= p then br else id) @@
          pretty_prio po l ^ " " ^ o ^ " " ^ pretty_prio po r
      end in gcata'expr transobj p e'
    in pretty_prio min_int e;;
end;;
      
(* a transformation object can also be obtained from
   instances of a virtual transformation class *)
class virtual ['iota, 'sigma] transformation'expr =
  object
    method virtual pConst : 'iota -> int -> 'sigma
    method virtual pVar : 'iota -> string -> 'sigma
    method virtual pBinop : 'iota -> string -> expr -> expr -> 'sigma
  end;;


module ExprCls =
struct
  (* a concrete transformation class for show'expr *)
  (* a self-trasformation function fself facilitates recursive calls *)
  class show (fself : expr -> string) =
    object
      inherit [unit, string] transformation'expr
      method pConst _ n = "Const " ^ string_of_int n
      method pVar _ x = "Var " ^ x
      method pBinop _ o l r =
        Printf.sprintf "Binop (%S, %s, %s)" o (fself l) (fself r)
    end;;

  (* concrete transformation class *)
  class pretty (fself : 'iota -> expr -> 'sigma) =
    object
      inherit [int, string] transformation'expr
      method pConst _ n = string_of_int n
      method pVar _ x = x
      method pBinop p o l r =
        let po = prio o in
        (if po <= p then br else id) @@
        fself po l ^ " " ^ o ^ " " ^ fself po r
    end;;

  let rec show'expr e = gcata'expr (new show show'expr) () e;;
  
  let pretty'expr e =
    let rec pretty_prio p e = gcata'expr (new pretty pretty_prio) p e in
    pretty_prio min_int e;;

end;;

(* tests *)

let expr1 = Binop ("+", Const 1, Var "x");;
let expr2 = Binop ("exp",
                   (Binop ("*", (Const 4), (Var "x"))),
                   (Binop ("-",
                           (Var "y"),
                           (Binop ("+",(Const 4),(Var "z"))))));;


print_string (ExprPrint.show'expr expr1);;
print_string (ExprImO.show'expr expr1);;
print_string (ExprCls.show'expr expr1);;

ExprCls.pretty'expr expr2;; 
ExprPrint.pretty'expr expr2;; 
ExprImO.pretty'expr expr2;;


(* the fold transfomration class *)

class ['iota] fold'expr =
  fun (fself : 'iota -> expr -> 'iota) ->
  object
    inherit ['iota, 'iota] transformation'expr
    method pConst i _ = i
    method pVar i _ = i
    method pBinop  i o l r = fself (fself i l) r
  end;;

let rec thread_through () e = gcata'expr (new fold'expr thread_through) () e;;
thread_through () expr1;;
thread_through () expr2;;

(* Inheriting the fold transformer, 
   with light modification, we can: *)

(* get the list of all free arithmetic variables *)

let fv e =
  let rec fv' = fun i e ->
    let foldobj =
      object
        inherit [string list] fold'expr fv'
        method! pVar i x = x :: i
      end
    in gcata'expr foldobj i e
  in fv' [] e;;

fv expr1;;
fv expr2;;

(* get the height of an expression's syntax tree *)

class height (fself : int -> expr -> int) =
  object
    inherit [int] fold'expr fself
    method! pBinop i _ l r = 1 + max (fself i l) (fself i r)
  end;;

let height =
  let rec height' i e' = gcata'expr (new height height') i e' in
  height' 0;;

height expr1;;
height expr2;;

class map'expr fself =
  object
    inherit [unit, expr] transformation'expr
    method pVar _ x = Var x
    method pConst _ n = Const n
    method pBinop _ o l r = Binop (o, fself () l, fself () r)
  end;;

let rec id'expr () e = gcata'expr (new map'expr id'expr) () e;;
ExprImO.pretty'expr (id'expr () expr3);;


class simplify fself =
  object
    inherit map'expr fself
    method! pBinop _ o l r =
      match fself () l, fself () r with
      | Const m, Const n -> Const ((op o) m n)
      | le, re -> Binop (o, le, re)
  end;;
  
let rec simplify'expr () e = gcata'expr (new simplify simplify'expr) () e;;

let expr3 = Binop ("exp",
                   (Binop ("*", (Const 4), (Const 2))),
                   (Binop ("-",
                           (Var "y"),
                           (Binop ("+",(Const 4),(Const 4))))));;

ExprImO.pretty'expr expr3;;
ExprImO.pretty'expr (simplify'expr () expr3);;

class substitute fself state =
  object
    inherit map'expr fself
    method pVar _ x = Const (state x)
  end;;

let rec substitute'expr () =
  gcata'expr (new substitute substitute'expr state) ();;

ExprImO.pretty'expr expr2;;
ExprImO.pretty'expr (substitute'expr () expr2);;
ExprImO.pretty'expr (simplify'expr ()  (substitute'expr () expr2));;

class eval fself state =
  object
    inherit map'expr fself
    method pVar _ x = Const (state x)
    method! pBinop _ o l r =
      match fself () l, fself () r with
      | Const m, Const n -> Const ((op o) m n)
      | le, re -> Binop (o, le, re)
  end;;

let evalue e state =
  let rec eval'expr () e' = gcata'expr (new eval eval'expr state) () e' in
  eval'expr () e;;

ExprImO.pretty'expr expr2;;
ExprImO.pretty'expr (evalue expr2 state);;

(* Tracking class inheritance

transformation'expr (virtual)
   /    |      \            \      
  /     |       \            \
show  pretty  fold'expr      map'expr
                /   \         /    \
               /     \       /      \
              fv   height simplify substitute
                               \    /
                                \  /
                                eval

*)
                              
(* Recursive module *)

module rec A : sig
  type t = Leaf of string | Node of ASet.t
  val compare : t -> t -> int
end = struct
  type t = Leaf of string | Node of ASet.t
  let compare t1 t2 =
    match (t1, t2) with
    | (Leaf s1, Leaf s2) -> Stdlib.compare s1 s2
    | (Leaf _, Node _) -> 1
    | (Node _, Leaf _) -> -1
    | (Node n1, Node n2) -> ASet.compare n1 n2
end
and ASet
  : Set.S with type elt = A.t
  = Set.Make(A)
;;

(* The definitions of the module A and the module Aset are 
   mutually dependent *)


(* Private types *)

(* Private type representation allows pattern matching 
   but not using value constructor application to create
   values of that type *)

module M : sig
  type t = private
    | A
    | B of int
  val a : t
  val b : int -> t
end = struct
  type t = A | B of int
  let a = A
  let b n = assert ( n > 0 ) ; B n
end
;;

M.a;;
M.b 5;;
let string_of_M = function
  | M.A -> "A"
  | M. B n -> "B " ^ string_of_int n
;;

string_of_M (M.b 99);;
string_of_M (M.a);;

module M': sig
  type t = private
    { name : string;
      date_of_birth : int * int * int;
      mutable service_age : int }
  val create_record : string ->  int * int * int -> int -> t
    val update_service_age : t -> int -> unit
end = struct
  type t = { name : string;
             date_of_birth : int * int * int;
             mutable service_age : int }
  let create_record s dob sa =
    {name = s; date_of_birth = dob; service_age = sa}
  let update_service_age rc sa =
    rc.service_age <- if sa > rc.service_age
      then sa else raise (Invalid_argument "update_service_age")
end;;

                               
let myrecord = M'.create_record "Yue Li" (12, 12, 2020) 4;;
(* fields of a private record type can be accessed normally *)
myrecord.name;;
myrecord.service_age;;
myrecord.date_of_birth;;

(* we can also pattern match against a private record type *)
let post80s : M'.t -> bool = function
    {date_of_birth = (_, _, y)} when (y > 1980) -> true
                                   | _ -> false
;;
post80s myrecord;;

M'.update_service_age myrecord 10; myrecord;;

(* Direct mutable field modification and record 
   creatiion is not allowed for a private record type 
 
myrecord.service_age <- 100;;
({ name = "James"; date_of_birth = (1,2,3); service_age = 14 } : M'.t );;

*)

(* Private type abbreviation *)

module N : sig
  type t = private int
  val of_int : int -> t
  val to_int : t -> int
end = struct
  type t = int
  let of_int n = assert (n >= 0); n
  let to_int n = n
end;;

(* type t = private int 
   can be read as "t is a restricted form of int". 
   This implies that t can always be coerced into int but otherwis they 
   are not automatcally considered as equal *)

( (N.of_int 8) : N.t :> int );;

(* type error 

   (N.of_int 8) = 8;; *)

let nums = List.map N.of_int [1;2;3;4;5];; 
(nums : N.t list :> int list);; 


(* private row types: i.e. object or polymorphic variant type *)

module Ma =
struct
  class c = object method x = 3 method y = 2 end
  let o = new c
end;;

(*
module Ma :
  sig class c : object method x : int method y : int end val o : c end
*)

module Mb : sig
type c = private < x : int; .. > val o : c end =
struct
  class c = object method x = 3 method y = 2 end
  let o = new c
end;;

(* M.c is not recognized as a class
let obj = new M.c;;
*)

module F(X : sig type c = private < x : int; .. > end) =
struct
  let get_x (o : X.c) = o # x
end;;

module FMa = F(Ma);;
module FMb = F(Mb);;

FMa.get_x Ma.o;;
FMb.get_x Mb.o;;

module G(X : sig type c = private < x : int; y : int; .. > end) =
struct
  include F(X)
  let get_y  (o : X.c) = o # y
end;;

module GMa = G(Ma);;
GMa.get_x (Ma.o), GMa.get_y (Ma.o);;

(* signature mismatch 

module GMb = G(Mb);;

*)

(* declaring an object type private we can prevent creation of
   new objects of such type *)

type t = [`A of int | `B of bool]

(* extend t with hidden new tags *)

type u = private [> t];;

let f : u -> string = function
  | `A n -> "A " ^ string_of_int n
  | `B n -> "B " ^ string_of_bool n
  | _ -> "abstract constructor";; 

f (`A 100), f (`B true);;

(* we must have the last case in pattern matching for unkonwn tags,
   but we cannot supply such a tag to f *)

(* compare  f,  f' and f'': their types and the way they can be applied  *)
let f' = function
  | `A n -> "A " ^ string_of_int n
  | `B n -> "B " ^ string_of_bool n
;;
(*
val f' : [< `A of int | `B of bool ] -> string = <fun>
*)

let f'' = function
  | `A n -> "A " ^ string_of_int n
  | `B n -> "B " ^ string_of_bool n
  | _ -> "abstract tag"
;;
(*
val f'' : [> `A of int | `B of bool ] -> string = <fun>
*)
f'' (`C 'a');;

(* f is somewhat in between f' and f'': it has an open type, yet 
   we cannot supply tags that are not specified explicitly in the 
   pattern matching block. Annotaing f with u modifies the behaviour 
   of f. *)

(* error
type u' = [> t];;
*)

(* intuitively, not more than t but not less than `A *)
type v = private [< t > `A];;

(* values of type v can only be constructe using 
   the listed tags, viz., `A *)
(`A 100 : v );;

(* When pattern matching on v, we must assume that all tags 
   of t may be present *)
let g : v -> string = function
  | `A n -> "A " ^ string_of_int n
  | `B b -> "B " ^ string_of_bool b
;;

(*  `B true is an invalid value
    g (`B true);; *)

g (`A 100);;
