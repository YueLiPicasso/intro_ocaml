

type expr = Con of int
          | Var of string
          | Arr of string * expr
          | Brh of expr * expr * expr;;

type value = Conv of int | Arrv of int list;;

let rec assoc : string -> (string * value) list -> value = fun key list -> 
  match list with
  | (k, v) :: t -> if k = key then v else assoc key t;;

let rec nth : value -> value -> value = fun arg1 arg2 ->
  match arg1, arg2 with
    (Arrv l),  (Conv n) ->
    begin
      match l with
      | h :: t -> if n = 0 then Conv h else nth (Arrv t) (Conv (n-1))
    end;; 

let rec eval_imp : (string * value) list -> expr -> value = fun s e -> 
  match e with
  | Con c -> Conv c
  | Var name -> assoc name s
  | Arr (name, index) -> nth (assoc name s) (eval_imp s index);;



                         
