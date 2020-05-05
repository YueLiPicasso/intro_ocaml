open Syntax;;

(* big step reduction semantics *)

(* the tuorial's website and its Spinger 
   publication substantially diverge from
   eachother regarding the implementation
   of big step semantics. We follow the 
   website whch looks right *)


(* mutually recursive types *)

type env = (string * value) list
and value =
  | Closure of var * expr * env
  | Constant of constant * value list
;;


type answer = Error  of string
            | Value of value
;;

(* constructing a value from an integer *)
let val_int u =
  Value (Constant ({name = Int u;
                    arity = 0;
                   constr = true}, []));;

let delta c l =
  match c.name, l with
  | Name "+", [Constant({name = Int u},[]);Constant({name = Int v},[])] ->
    val_int (u + v)
  | Name "*", [Constant({name = Int u},[]);Constant({name = Int v},[])] ->
    val_int (u * v)
  | _ -> Error "EVAL-PRIM-ERROR"
;;

(* EVAL-VAR *)
let get x env =
  try Value (List.assoc x env) with Not_found ->
    Error ("Variable undefined: " ^ x)
;;



let rec eval env = function
  | Var x -> get x env  (* EVAL-VAR *)
  | Const c -> Value (Constant (c, []))
  | Fun(x,a) -> Value (Closure (x, a, env))  (* EVAL-FUN *)
  | Let (x, a1, a2) -> 
    begin match eval env a1 with
      | Value v1 -> eval ((x, v1)::env) a2   (* EVAL-LET *)
      | Error _ -> Error "EVAL-LET-ERROR"
    end
  | App(a1,a2) ->
    begin match eval env a1 with
      | Value v1 ->
        begin match v1, eval env a2 with
          | Constant (c,l), Value v2 ->
            let k = List.length l + 1 in
            if c.arity < k
            then Error "EVAL-APP-ERROR"
            else if c.arity > k  (* EVAL-CONST *)
            then Value (Constant (c, v2::l)) (* partial application *)
            else if c.constr
            then Value (Constant (c, v2::l)) (* constructor full application *)
            else delta c (v2::l)     (* primitive full application *)
          | Closure (x, e, env0), Value v2 -> eval ((x, v2)::env0) e
          | _, Error _ -> Error "EVAL-APP-ERROR-RIGHT/EVAL-CONST-ERROR"
        end
      | Error _ -> Error "EVAL-APP-ERROR-LEFT"
    end
;;


(* tests *)


(* The implementation does not provide a bound-variable renaming  mechanism 
   to prevent the environment from have multiple entries with the same key.
   However, duplicated variable names are handled correctly, each receiving 
   the right value due to the fact that the environment is implemented as a 
   stack, with new key-value pairs added at the top, and the correct stack state
   is recovered upon function call return. *)

let env = [("x", Constant({name = Int 2; constr = true; arity = 0},[]));
           ("y", Constant({name = Int 5; constr = true; arity = 0},[]));
           ("t", Constant({name = Name "*"; constr = false; arity = 2},[]));
           ("z", Closure("x", App(plus, Var "x"), []))];;


eval env (Var "x");;
eval env (Var "y");;
eval env (Var "t");;
eval env (Var "z");;
eval env (Var "x1");;
eval env (App(Var "x", int 10));;
eval env (App(Var "t", int 10));;
eval env (App(Var "z", int 10));;
eval env (App(App(Var "z", int 10), Var "x"));;
eval env (App(App(Var "z", Var "x"), int 10));;
eval env (App(App(Var "z", Var "x"), Var "x"));;

(* The scope of a bound variable is clear from the abstract syntax *)

eval env (Let ("x", int 5, (App(App(Var "z", Var "x"), Var "x"))));;
eval env (App(Fun("x", (Let("x", int 5, Var "x"))), int 4));;
eval env (App(Fun("x",App(App(plus, Let("x", int 5, Var "x")), Var "x")), int 4));;

