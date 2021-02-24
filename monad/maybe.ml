module DivOpt = struct
  
  let div (x:int) (y:int) : int option =
    if y = 0 then None else Some (Stdlib.( / ) x y);;

  let ( / ) x y = div x y;;
  
end;;

module ArithOpt = struct
  
  let plus_opt (x : int option) (y : int option) : int option =
    match x,y with
    | None, _ | _, None -> None
    | Some a , Some b -> Some (Stdlib.( + ) a b);;

  let minus_opt (x : int option) (y : int option) : int option =
    match x,y with
    | None, _ | _, None -> None
    | Some a , Some b -> Some (Stdlib.( - ) a b);;

  let mult_opt (x : int option) (y : int option) : int option =
    match x,y with
    | None, _ | _, None -> None
    | Some a , Some b -> Some (Stdlib.( * ) a b);;

  let div_opt (x : int option) (y : int option) : int option =
    match x,y with
    | None, _ | _, None -> None
    | Some a , Some b -> if b = 0
                         then None
                         else Some (Stdlib.( / ) a b);;

  let ( + ) = plus_opt;;
  let ( - ) = minus_opt;;
  let ( * ) = mult_opt;;
  let ( / ) = div_opt;;

end;;

module ArithOpt_Abs = struct
  
  let propagate_none (op : int -> int -> int) (x : int option) (y : int option) : int option =
    match x,y with
    | None, _ | _, None -> None
    | Some a , Some b -> Some (op a b);;

  let div_opt (x : int option) (y : int option) : int option =
    match x,y with
    | None, _ | _, None -> None
    | Some a , Some b -> if b = 0
                         then None
                         else Some (Stdlib.( / ) a b);;

  let ( + ) = propagate_none Stdlib.( + );;
  let ( - ) = propagate_none Stdlib.( - );;
  let ( * ) = propagate_none Stdlib.( * );;
  let ( / ) = div_opt;;
end;;

module Arith_Opt = struct
  
  let propagate_none (op : int -> int -> int option) (x : int option) (y : int option) : int option =
    match x,y with
    | None, _ | _, None -> None
    | Some a , Some b -> op a b;;

  (* convert an [int -> int -> int] function to an [int -> int -> int option] function *)
  let wrap_output (op : int -> int -> int) (x:int) (y:int) = Some (op x y)

  let ( + ) = propagate_none @@ wrap_output Stdlib.( + );;
  let ( - ) = propagate_none @@ wrap_output Stdlib.( - );;
  let ( * ) = propagate_none @@ wrap_output Stdlib.( * );;
  let ( / ) = propagate_none DivOpt.( / );;
  
end;;


module ArithM = struct

  (* Convert an [int -> int -> int option] function to 
     an [int option -> int option -> int option] function *)
  let propagate_none (op : int -> int -> int option) (x : int option) (y : int option) : int option =
    match x,y with
    | None, _ | _, None -> None
    | Some a , Some b -> op a b;;

  (* convert an [int -> int -> int] function to an [int -> int -> int option] function *)
  let wrap_output (op : int -> int -> int) (x:int) (y:int) = Some (op x y)

  let div (x:int) (y:int) : int option =
    if y = 0 then None else wrap_output Stdlib.( / ) x y;;

  let ( + ) = propagate_none @@ wrap_output Stdlib.( + );;
  let ( - ) = propagate_none @@ wrap_output Stdlib.( - );;
  let ( * ) = propagate_none @@ wrap_output Stdlib.( * );;
  let ( / ) = propagate_none div;;
  
end;;

(* put a value into the metaphorical box *)
let return (x:int) : int option = Some x;;

(* the core work of upgrading from [int -> int option] to 
   [int option -> int option] *)
let bind (x : int option) (op : int -> int option) : int option =
  match x with
  | None -> None
  | Some a -> op a;;

let (>>=) = bind;;

let upgrade : (int -> int option) -> (int option -> int option) =
  fun op x -> (x >>= op);;

module ArithMo = struct
  
  let ( + ) (x : int option) (y : int option) : int option =
    x >>= fun a -> y >>= fun b -> return (Stdlib.( + ) a b);;

  let ( - ) (x : int option) (y : int option) : int option =
    x >>= fun x' -> y >>= fun y' -> return (Stdlib.( - ) x' y');;

  let ( * ) (x : int option) (y : int option) : int option =
    x >>= fun x' -> y >>= fun y' -> return (Stdlib.( * ) x' y');;

  let ( / ) (x : int option) (y : int option) : int option =
    x >>= fun x' -> y >>= fun y' ->
              if y' = 0 then None else return (Stdlib.( / ) x' y');;
end;;

module ArithMon = struct
  
  let upgrade_binary :
        (int -> int -> int option) -> (int option -> int option -> int option)
    = fun op x y -> x >>= fun x' -> y >>= fun y' -> op x' y';;

  let return_binary :
        (int -> int -> int) -> (int -> int -> int option)
    = fun op x y -> return (op x y);;

  let ( + ) = upgrade_binary (return_binary Stdlib.( + ));;
  let ( - ) = upgrade_binary (return_binary Stdlib.( - ));;
  let ( * ) = upgrade_binary (return_binary Stdlib.( * ));;
  let ( / ) = upgrade_binary DivOpt.( / );;
  
end;;

module type Monad = sig
  type 'a t;;
  val ( >>= ) : 'a t -> ('a -> 'a t) -> 'a t;;
  val return  : 'a -> 'a t;;
  val error   : 'a t;;
end;;

module Maybe : Monad = struct
  type 'a t = 'a option;;
  let error = None;;
  let return x = Some x;;
  let (>>=) x op = match x with
    | None -> None
    | Some x' -> op x';;
end;;

module Arith_Monadic = struct
  
  open Maybe;;
  
  let upgrade_binary :
        (int -> int -> int t) -> (int t -> int t -> int t)
    = fun op x y -> x >>= fun x' -> y >>= fun y' -> op x' y';;

  let return_binary :
        (int -> int -> int) -> (int -> int -> int t)
    = fun op x y -> return (op x y);;

  let return_binary' :
        (int -> int -> int) -> int -> int -> int t
    = fun op x y -> if y = 0 then error else return (op x y);;

  let ( + ) = upgrade_binary (return_binary  Stdlib.( + ));;
  let ( - ) = upgrade_binary (return_binary  Stdlib.( - ));;
  let ( * ) = upgrade_binary (return_binary  Stdlib.( * ));;
  let ( / ) = upgrade_binary (return_binary' Stdlib.( / ));;
  
end;;
