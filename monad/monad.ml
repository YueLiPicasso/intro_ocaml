(* textbook definition *)

module type Monad = sig
  type 'a t;;
  val return : 'a -> 'a t;;
  val bind : 'a t -> ('a -> 'b t) -> 'b t;;
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t;; (** same as [bind] *)
end;;


(* experimental definition *)
module type TC1 =
  sig
    type 'a t;;
  end;;

module type Monad =
  functor (M : TC1) -> sig
    val return : 'a -> 'a M.t;;
    val (>>=)  : 'a M.t -> ('a -> 'b M.t) -> 'b M.t;;
  end;;

module Maybe (M : TC1) : Monad =
  struct
    type 'a M.t = 'a option;;
    let return x = Some x;;
    let (>>=) = fun x f ->
      match x with
      | None -> None
      | Some x' -> f x';;
  end;;


module Int = struct
  type 'a t = 
  end;;
mpdule M1 = Maybe(Int);
mpdule M1 = Maybe(Bool);
