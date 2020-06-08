(* Recursive values *)

let rec oz = 1 :: zo and zo = 0 :: oz in (oz, zo);;
(* 
- : int list * int list = ([1; 0; <cycle>], [0; 1; <cycle>]) 
*)

(* GT examples *)
type expr =
  | Const of int
  | Var of string
  | Binop of string * expr * expr;;

let rec show = function
  | Const n -> "Const " ^ string_of_int n
  | Var x -> "Var " ^ x
  | Binop (o, l, r) ->
    Printf.sprintf "Binop (%S, %s, %s)" o (show l) (show r);;

print_string (show (Binop ("+", Const 1, Var "x")));;

(* simple priority assignment *)
let prio = function
    "+" -> 1
  | "-" -> 2
  | "*" -> 3
  | "/" -> 4
  | "exp" -> 5
  | s -> raise (Invalid_argument s);;

let id : 'a. 'a -> 'a = fun x -> x;;

let br = fun (s : string) -> "( " ^ s ^ " )";;

br "hello";;

let pretty e =
  let rec pretty_prio p = function
    | Const n -> string_of_int n
    | Var x -> x
    | Binop (o, l, r) ->
      let po = prio o in
      (if po <= p then br else id) @@
      pretty_prio po l ^ " " ^ o ^ " " ^ pretty_prio po r
  in
  pretty_prio min_int e;;

pretty (Binop ("exp", (Binop ("*", (Const 4), (Var "x"))),
               (Binop ("-", (Var "y"), (Binop ("+",(Const 4),(Var "z")))))));; 

(* extract pattern matching from "pretty" and "show" *)

let gcata omega iota = function
  | Const n -> omega # pConst iota n
  | Var x -> omega # pVar iota x
  | Binop (o, l, r) -> omega # pBinop iota o l r;;
(* omega is a transformation object *)
(* iota is an extra parameter that can be used or ignored *)

let rec show' e =
  let transobj =
  object
    method pConst _ n = "Const " ^ string_of_int n
    method pVar _ x = "Var " ^ x
    method pBinop _ o l r =
      Printf.sprintf "Binop (%S, %s, %s)" o (show' l) (show' r)
  end
  in gcata transobj () e;;
  
print_string (show' (Binop ("+", Const 1, Var "x")));;


class virtual ['iota, 'sigma] expr_trans =
  object
    method virtual pConst : 'iota -> int -> 'sigma
    method virtual pVar : 'iota -> string -> 'sigma
    method virtual pBinop : 'iota -> string -> expr -> expr -> 'sigma
  end;;

(*
let rec pretty' e =
  let transobj =
    object (self)
      method pConst _ n = string_of_int n
      method pVar _ x -> x
      method pBinop p o l r = let po = prio o in
      (if po <= p then br else id) @@
      pretty' l ^ " " ^ o ^ " " ^ pretty' r
    end
  in
  gcata transobj min_int e;;*)

        
