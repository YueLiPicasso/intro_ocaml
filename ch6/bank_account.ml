module type MONEY =
sig
  type t;;
  class c : float ->
    object ('a)
      val repr : t
      method value : t
      method print : unit
      method times : float -> 'a
      method leq : 'a -> bool
      method plus : 'a -> 'a
    end;;
end;;

module Euro : MONEY =
struct
  type t = float
  class c x =
    object (self : 'a)
      val repr = x
      method value = repr
      method print = print_float repr
      method times k = {< repr = k *. x >}
      method leq (p : 'a) = repr <= p#value
      method plus (p : 'a) = {< repr = x +. p#value >}
    end
end;;

let euro = new Euro.c;;

let zero = euro 0.;;

let neg x = x # times (-1.);;

class account =
  object
    val mutable balance = zero
    method balance = balance
    method deposit x = balance <- balance # plus x
    method withdraw x =
      if x # leq balance
      then (balance <- balance # plus (neg x); x)
      else zero
  end;;


let c = new account in c # deposit (euro 100.); c # withdraw (euro 50.);;


class account_with_interests =
  object (self)
    inherit account
    method private interest = self # deposit (self # balance # times 0.03)
  end;;


class safe_account =
  object
    inherit account as unsafe
    method deposit x =
      if zero # leq x then unsafe # deposit x
      else raise (Invalid_argument "deposit")
  end;;

(* keep track of account oprations *)
type 'a operation = Deposit of 'a | Retrieval of 'a;;

class account_with_history =
  object (self)
    inherit safe_account as super
    val mutable history = []
    method private trace x = history <- x :: history
    method deposit x = self # trace (Deposit x); super # deposit x
    method withdraw x = self # trace (Retrieval x); super # withdraw x
    method history = List.rev history
  end;;

class account_with_deposit x =
  object (self)
    inherit account_with_history
    initializer self # deposit x
  end;;

(* test the account *)
let ccp = new account_with_deposit (euro 100.) in
let _balance = ccp # withdraw (euro 50.) in
ccp # history;;
        
(* close account *)
let close c = c#withdraw c#balance;;

let today () = (01,01,2000)


(* Account functor abstracted over some currency *)

module Account (M:MONEY) =
struct
  type 'a operation = Deposit of 'a | Retrieval of 'a;;
  type m = M.c
  let m = new M.c
  let zero = m 0.
  let neg (x : M.c) = x # times (-1.)

  class bank =
    object (self)
      val mutable balance = zero
      method balance = balance
      val mutable history = []
      method private trace x = history <- x :: history
      method deposit x =
        self # trace (Deposit x);
        if zero # leq x then balance <- balance # plus x
        else raise (Invalid_argument "deposit")
      method withdraw x =
        if x#leq balance then
          (balance <- balance # plus (neg x); self # trace (Retrieval x); x)
        else zero
      method history = List.rev history
    end

  class type client_view =
    object
      method deposit : m -> unit
      method history : m operation list
      method withdraw : m -> m
      method balance : m
    end

  class virtual check_client x =
    let y = if (m 100.)#leq x then x
      else raise (Failure "Insufficient initial deposit") in
    object (self)
      initializer self # deposit y
      method virtual deposit : m -> unit
    end

  (* module Client (B : sig class bank : client_view end)*)
end;;

