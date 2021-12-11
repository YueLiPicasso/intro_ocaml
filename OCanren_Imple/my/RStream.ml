(* A stream t of values of type 'a is one of ... *)

type 'a t =
  | Nil                          (* an empty stream *)
  | Cons of 'a * ('a t)          (* a value consed to a stream *)
  | Thunk of 'a thunk            (* a thunk *)
  | Waiting of 'a suspended list (* a list of suspended thunks *)
and 'a thunk =                   (* A thunk is a function that .. *)
  unit -> 'a t      (* .. takes a unit value and returns a stream *)           
and 'a suspended =         (* A suspended thunk is .. *) 
  {zz: 'a thunk;           (* a thunk paired with .. *)
   is_ready: unit -> bool} (* .. a readiness indicator *)               
;;  

(* some stream builders .. *)

let nil : 'a t                  (* make an empty stream *)
  = Nil
;;  
let single : 'a -> 'a t         (* make a singleton stream *)
  = fun x -> Cons (x, Nil)
;;           
let cons : 'a -> 'a t -> 'a t   (* cons a value to a stream *)
  = fun x s -> Cons (x, s)
;;             
let from_fun : 'a thunk -> 'a t (* convert a thunk to a stream *) 
    = fun zz -> Thunk zz
;;
let suspend : is_ready:(unit -> bool) -> 'a thunk -> 'a t
  (* a singleton list of a suspended thunk *)
  = fun ~is_ready f -> Waiting [{is_ready; zz=f}]
;;
