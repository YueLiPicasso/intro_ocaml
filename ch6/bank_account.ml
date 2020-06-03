(* Simple bank account *)

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

  module Client (B : sig class bank : client_view end) =
  struct
    class account x : client_view =
      object
        inherit B.bank
        inherit check_client x
      end

    let discount x =
      let c = new account x in
      if today() < (30,10,1998) then c # deposit (m 100.); c
  end
end;;

module Euro_account = Account(Euro);;

module Client = Euro_account.Client (Euro_account);;

new Client.account (new Euro.c 100.);;

module Investment_account (M : MONEY) =
struct
  (*type m = M.c*)
  module A = Account(M)

  class bank =
    object
      inherit A.bank as super
      method deposit x =
        if (new M.c 1000.)#leq x then
          print_string "Would you like to invest?";
        super # deposit x
    end

  module Client = A.Client
end;;

module Euro_invest_acnt = Investment_account (Euro);;

module Invest_client = Euro_invest_acnt.Client(Euro_invest_acnt);;

new Invest_client.account (new Euro.c 19000.);;


module Internet_account (M : MONEY) =
struct
  module A = Account (M)

  class bank =
    object
      inherit A.bank
      method mail s = print_string s
    end

  class type client_view =
    object
      inherit A.client_view
      method mail : string -> unit
    end

  module Client (B : sig class bank : client_view end) =
  struct
    class account x : client_view =
      object
        inherit B.bank
        inherit A.check_client x
      end
  end
end;;


module Yue_Net = Internet_account (Euro);;  
  
module Yue_Net_Client = Yue_Net.Client(Yue_Net);;

let myacc =  new Yue_Net_Client.account (new Euro.c 100.);;
myacc # history;;
myacc # withdraw (new Euro.c 22.);;


(* Simple modules as classes *)

(* Strings *)

class ostring s =
  object
    method get n = String.get s n
    method print = print_string s
    method escaped = new ostring (String.escaped s)
  end;;

(* the method escaped returns an object of the class ostring, 
   not an object of the current class, so that it does not update
   itself when the class ostring is inherited *)

class sub_string s =
  object
    inherit ostring s
    method sub start len = new sub_string (String.sub s start len)
  end;;

    
(new sub_string "hello\n\t") # escaped;; (* object of class ostring *)

(* use object clone {< >} to support scalability *)

class better_string s =
  object
    val repr = s
    method get n = String.get repr n
    method print = print_string repr
    method escaped = {< repr = String.escaped repr>}
    method sub start len = {< repr = String.sub s start len >}
  end;;

(new better_string "hello\nworld") # escaped # print;;
(new better_string "hello\nworld") # print;;
(new better_string "hello\nworld") # escaped;; (* : better_string *)

class oString s =
  object
    inherit better_string s
    method make len ch = {< repr = String.make len ch >}
  end;;

(new oString "hello\nworld") # escaped;; (* : oString *)
((new oString "hello\nworld") # make 5 'c') # print;;
