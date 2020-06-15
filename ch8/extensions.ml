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


(* Locally abstract types *)

(*  pseudo type parameters themselves does not suspend the 
    evaluation of the body of the abstraction *)
fun (type t) -> 1;;

(* type constructors introduced using ( type ... ) is considered 
   abstract (no representation, no equation, incompatible with 
   any other type) in the scope of the sub-expresion, but will then
   be replaced by a fresh type variable after type checking  *)
let foldl = fun (type t) (g : int -> t -> int) init (foo : t list) ->
  List.fold_left g init foo;;
(*
val foldl : (int -> 'a -> int) -> int -> 'a list -> int 
*)

(* compare : *)
(* This does not pass type-checking
let plu = fun (type t) (l : t) (r : t) -> l + r;; *)
let plu = fun (l : 't) (r : 't) -> l + r;;

let foldl_tpl (type a b c)
    (g : a -> b * c -> a) (ini : a) (l : (b * c) list) =
  List.fold_left g ini l;;

(* locally abstract types can be supplied to local modules 
   to get polymorphic functions *)

let f (type t) () =
  let module M = struct exception E of t end in
  (fun x -> M.E x), (function M.E x -> Some x | _ -> None);;
f ();;


let sort_uniq (type s) (cmp : s -> s -> int) =
  let module S = Set.Make(struct type t = s let compare = cmp end) in
  fun l -> S.elements (List.fold_right S.add l S.empty);;

module IntSet = Set.Make(struct type t = int let compare = Stdlib.compare end);;

let to_set : int list -> IntSet.t =
  fun l -> List.fold_right IntSet.add l IntSet.empty;;

let myset = to_set [1;1;1;1;2;2;3;4;4;4;5;6;6;7;7;7;7] in
IntSet.elements myset;;

sort_uniq Stdlib.compare [1;1;1;1;2;2;3;4;4;4;5;6;6;7;7;7;7];;
sort_uniq Stdlib.compare ['a';'b';'a';'a';'c'];;


(* First-class module *)

(* Basic example *)

type picture = Apple | Banana | Cherry;;

(* we get several implementations of the same module type *)

module type DEVICE = sig
  val draw : picture -> unit
end;;

module SVG = struct
  let draw : picture -> unit = function
    | Apple -> print_string "Apple\n"
    | Banana -> print_string "Banana\n"
    | Cherry -> print_string "Cherry\n"
end;;

module PDF = struct
  let draw : picture -> unit = function
    | Apple -> print_string "APPLE\n"
    | Banana -> print_string "BANANA\n"
    | Cherry -> print_string "CHERRY\n"
end;;

(* ... these implementations are stored in a hash table 
   as first-class modules *)

let devices : (string, (module DEVICE)) Hashtbl.t = Hashtbl.create 17;;
Hashtbl.add devices "SVG" (module SVG : DEVICE);;
Hashtbl.add devices "PDF" (module PDF : DEVICE);;  

(* ... we can then select an implementation as run-time *)

let draw_using_device device_name picture =
  let module Device = (val (Hashtbl.find devices device_name) : DEVICE) in
  Device.draw picture;;

draw_using_device "PDF" Apple;;
draw_using_device "SVG" Apple;;

(* Advanced example *)

let sort (type s) (module SSet : Stdlib.Set.S with type elt = s) l =
  SSet.elements (List.fold_right SSet.add l SSet.empty);;

(* the test *)

let module CharSet =
  Stdlib.Set.Make(struct type t = char let compare = Stdlib.compare end)
in
sort (module CharSet) ['d';'c';'b';'a'];;

(* the test revisited *)

(* taking a comparison function for certain type, returns
   a packaged module implementing the set of values of this
   type *)

let make_set (type s) (cmp : s -> s -> int) =
  let module OrdS = struct type t = s let compare = cmp end in 
  let module MySet = Stdlib.Set.Make(OrdS) in
  (module MySet : Stdlib.Set.S with type elt = s)
;;

sort (make_set Stdlib.compare)  ['d';'c';'b';'a'];;
sort (make_set Stdlib.compare)  ["hello";"great";"sky";"apple"];;
sort (make_set Stdlib.compare)  [3.43;2.33;1.22;0.99];;



(* Recovering the type of a module *)

(* The idea is to use the |module type of <module-expr>| syntactic form to 
   recover/infer/compute the signature of the module <module-expr>, in order 
   to re-define part of an existing module or to re-implement an existing module. 
   
   The main semantical feature of this form is "non-strengthing": for abstract 
   types and new datatypes (i.e. new record or variant types) from the module 
   <module-expr>, the inferred signature does not add type equations to such types
   to emphasize that this type has an equal from <module-expr>.  

   The OCaml 4.10 Reference Manual section 8.7 gives two examples: one example shows 
   how to circumvent the "non-strengthing" feature so that a strengthened version 
   of the signature can be obtained for the purpose of re-definition of part of an 
   existing module; the other example shows how to exploit the "non-strengthing" 
   feature to re-implement an existing module. 

   The problems I found with this section are that:

   1. For the first example of Hashtbl (a standard library module), using or 
   not using the technique taught by the tutorial does not make any difference
   in terms of circumventing the "non-strengthing" feature;

   2. For the second example, applying the technique taught from the first example 
   causes a result that contains a syntactic form that does not agree with the language
   syntax (as far as I know)

*)

(* Ref Man's Example 1 *)

module type MYHASH = sig
  include module type of struct include Hashtbl end
  val replace : ('a, 'b) t -> 'a -> 'b -> unit
end;;

module MyHash : MYHASH = struct
  include Hashtbl
  let replace t k v = remove t k; add t k v
end;;

let mh = MyHash.create 10;;
MyHash.add mh "james" (24, 6, 1990);;
MyHash.find mh "james";;
MyHash.replace mh "james" (30, 12, 1999);;
MyHash.find mh "james";;

(* ... But why not just this: *)

module type MYHASH' = sig
  include module type of Hashtbl
  val replace :  ('a, 'b) t -> 'a -> 'b -> unit
end;;

module MyHash' : MYHASH' = struct
  include Hashtbl
  let replace t k v = remove t k; add t k v
end;;

let mh' = MyHash'.create 10;;
MyHash'.add mh "james" (24, 6, 1990);;
MyHash'.find mh "james";;
MyHash'.replace mh "james" (30, 12, 1999);;
MyHash'.find mh "james";;

(* Ref Man's Example 2 *)

module type MYSET =
sig
  include module type of Set
end;;

module type MYSET' =
sig
  include module type of struct include Set end
end;;

(* The module type MYSET is identical to the signature of Set, as expected *)
(* Problem: MYSET' that contains a specification 
   module Make = Set.Make
   which does not belong to any syntactic form given in 7.10 of Ref Man v4.10 *)


(* SOME EXPLORATION *)


(* Re-exported variant type *)

type mytp =  Hi of int | There of bool;;
type mytp' =  mytp = Hi of int | There of bool;;

let f : mytp -> unit = fun _ -> ();;

Hi 5;; (*: mytp' *)

f (Hi 5);; (* we get ()*)

(Hi 5 :> mytp);; (* successful *)

(* mytp' re-exports mytp: these two type constructors are 
   identified and they must have:
   - the same kind of representation (record or variant) 
   - exactly the same constructors/ fields 
   - - in the same order
   - - with the same arguments 
   - the same arity and type constraints (N/A for this example)
*)

(* semantics of signature-level module inclusion: 
   - simply performs textual copying
   - overriding is possible  *)

module type A = sig
  type t = Hello | World
  type u = int
  type v
  type w = mytp = Hi of int | There of bool
  val b : int -> int -> int
  val c : int
end;;

module type B = sig
  include A
  val a : bool
  val b : char
end;;

(* semantics of structure-level module inclusion *)

module Aa : A = struct
  type t = Hello | World
  type u = int
  type v = Edinburgh | Castle
  type w = mytp = Hi of int | There of bool
  let b = ( + )
  and c = 3
end;;

(* The module type A, when applied to the structure named Aa, 
   just makes the type v of Aa abstract; all other definitions 
   of Aa have their full information available to the user *)

module Bb =
struct
  include Aa
end;;

Bb.b 1 2 = 3;;

let f : Bb.t -> unit = fun _ -> ();;
f Aa.World;;

(* Bb re-exports the type t of Aa, for which Aa has a representation, in the way that: 
   - the representation is copied, and
   - a type equation is added, as 
   type t = Aa.t = Hello | World

   Bb also has the equations:
   type u = int
   which is simply copied from Aa, and 

   type v = Aa.v
   which is no longer abstract in Bb.

   value declarations are copied from Aa.    
*)

(* Particular attention shall be paid to type definitions in structure-level inclusion. 

   New variant/record type (no equation, a representation)
   ---|after inclusion|----> 
   re-exported variant/record type (an equation, a representation)

   Abstract type (no equation, no representation) 
   ---|after inclusion|----> 
   Type abbreviation (an equation, no representation)

   Type abbreviation 
   ---|after inclusion|----> 
   (the same) Type abbreviation 

   re-exported variant/record type
   ---|after inclusion|----> 
   (the same) re-exported variant/record type 
*)

(* Now compare B' with B'' and B''' *)

module type B' = sig
  include module type of Aa
end;;

module type B'' = sig
  include module type of struct include Aa end
end;;

module type B''' = sig
  include module type of Bb
end;;


(* The module type of Bb equals B'' and B''', 
   whilst the module type of Aa equals A and B'
*)

(* THE PROBLEMS *)

(* By "strengthening" we mean that asbtract types and datatypes (new records 
   and variants) from the included module are explicitly related to the included module, 
   for example, in Bb the types t and v are strengthened.   

   It is curious to see that: strengthening is absent for custom modules (as in B')  
   and the standart module Set (as in MySet) as prescribed by the language semantics, 
   but for the standard module Hastble strengthening presents, as in MYHASH'.  *)

(* END OF EXPLORATION *)
