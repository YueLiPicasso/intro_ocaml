module type Monad = sig
  type 'a t;;
  val return  : 'a -> 'a t;;
  val error   : 'a t;;
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t;;
end;;

module When (M : Monad) = struct
  let f = fun b m -> if b then m else M.return ();;
end;;

module Maybe = struct
  type 'a t = 'a option;;
  let return x = Some x;;
  let error = None;;
  let (>>=) = fun x f ->
    match x with
    | None -> None
    | Some x' -> f x';;
end;;

module List  = struct
  type 'a t = 'a list;;
  let return x = [x];;
  let error = [];;
  let rec (>>=) = fun x f ->
    match x with
    | [] -> []
    | x' :: xs -> f x' @ (xs >>= f);;
end;;

module M1 = When(Maybe);;
module M2 = When(List);;


module Unless(M : Monad) = struct
  module W = When(M);;
  let f b m = W.f (not b) m;;
end;;

module U1 = Unless(Maybe);;
(* Not work : *)
(* let myWhen (module M : Monad) b m =
  if b then m else M.return ();; *)

(* whenf can be factored to become the When functor *) 
(* 
module type Monadw = sig
  type 'a t;;
  val return  : 'a -> 'a t;;
  val error   : 'a t;;
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t;;
  val whenf   :  bool -> unit t -> unit t
end;;
*)
