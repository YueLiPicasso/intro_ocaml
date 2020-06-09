(* Recursive values *)

let rec oz = 1 :: zo and zo = 0 :: oz in (oz, zo);;
(* 
- : int list * int list = ([1; 0; <cycle>], [0; 1; <cycle>]) 
*)

(* GT examples *)

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

(* Tracking class inheritance

class transformation'expr (virtual)
        |          |           \
        |          |            \
 class show   class pretty  class fold'expr
                               |      \
                               |       \
                            class fv  class height

*)
                              
