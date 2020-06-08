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


class ostring s =
  object (self : 'str_obj)
    val repr = s
    method repr = repr
    method get n = String.get repr n
    method print = print_string repr
    method escaped = {< repr = String.escaped repr >}
    method sub start len = {< repr = String.sub repr start len >}
    method concat (t : 'str_obj) = {< repr = repr ^ t#repr>}
  end;;

let mystr = new ostring "Hello\tWorld !";;
mystr#print;;
mystr#escaped#print;;
let cat = mystr#concat (new ostring " Nice\n weather !");;
cat#print;;

(* constructor class for strings *)
class cstring n = ostring (String.make n ' ');;

(* The type implication of the above class definition *)
let cstr = new cstring 10;; (* : cstring *)
cstr#print;;
(* where does the method print come from? *)
(* Answer: we have the following specification for cstring:
   class cstring : int -> ostring *)

let catc = cstr#concat (new ostring "hello");; (* : cstring *)
(* why can you concat cstring with ostring? 
   Why cstring has the method concat anyway? *)
catc#print;;
let cstr' = new cstring;; (* : int -> cstring *)
cstr' 4;; (* : cstring*)
(cstr' 4 : cstring :> ostring);; (* this is correct *)

(* stack *)

exception Empty;;

class ['a] stack =
  object
    val mutable l = ([] : 'a list)
    method push x = l <- x :: l
    method pop = match l with [] -> raise Empty | a :: l' -> l <- l'; a
    method clear = l <- []
    method length = List.length l
    method fold : 'b. ('b -> 'a -> 'b) -> 'b -> 'b
      = fun f x -> List.fold_left f x l
  end;;

let bs = new stack;; 
bs # push true;; bs # push false;;


let f = fun c b -> match c with
  | 't' ->
    begin
      match true && b with
      | true -> 't'
      | false -> 'f'
    end
  | 'f' ->
    begin
      match false || b with
      | true -> 't'
      | false -> 'f'
    end
  | _ -> raise (Invalid_argument "f")
in bs # fold f 'f';;


(* hash table class *)

type hash_table_shape = Simple of int | Histo of int list;;

class type ['a, 'b] hash_table =
  object 
    method find : 'a -> 'b
    method add : 'a -> 'b -> unit
    method shape : hash_table_shape
  end;;

(* small hash table is implemented as an association list *)
class ['a, 'b] small_hashtbl : ['a, 'b] hash_table =
  object
    val mutable table = []
    method find key = List.assoc key table
    method add key valeur = table <- (key, valeur) :: table
    method shape = Simple (List.length table)
  end;;

let htb = new small_hashtbl;;
htb # add 1 "john";;
htb # add 2 "tom";;
htb # add 2 "jim";;
htb # find 2;;
htb # shape;;

(* a true hash table *)
(* size is the number of buckets *)
class ['a, 'b] hashtbl size : ['a, 'b] hash_table =
  object (self)
    val table = Array.init size (fun i -> new small_hashtbl)
    method private hash key =
      (Hashtbl.hash key) mod (Array.length table)
    method find key = table.(self#hash key) # find key
    method add key = table.(self#hash key) # add key
    method shape = let rec build_shape n shp =
                     if n < size
                     then  let Simple len = table.(n) # shape in
                       build_shape (n + 1) (len :: shp)
                     else shp in
      Histo (List.rev (build_shape 0 []))
  end;;

let hat = new hashtbl 10;;
hat # add "George Washington" (1789, 1797);;
hat # add "John Adams" (1797, 1801);;
hat # add "Thomas Jefferson" (1801, 1809);;
hat # add "James Madison" (1809, 1817);;
hat # add "James Monroe" (1817, 1825);;
hat # add "John Quincy Adams" (1825, 1829);;
hat # add "Andrew Jackson" (1829, 1837);;
hat # add "Martin Van Buren" (1837, 1841);;
hat # add "William Henry Harrison" (1841, 1841);;
hat # add "John Tyler" (1841, 1845);;
hat # add "James K. Polk" (1845, 1849);;
hat # add "Zachary Taylor" (1849, 1850);;
hat # add "Millard Fillmore" (1850, 1853);;
hat # add "Franklin Pierce" (1853, 1857);;
hat # add "James Buchanan" (1857, 1861);;
hat # add "Abraham Lincoln" (1861, 1865);;
hat # add "Andrew Johnson" (1865, 1869);;
hat # add "Ulysses S. Grant" (1869,1877);;
hat # add "Rutherford B. Hayes" (1877, 1881);;
hat # add "James A. Garfield" (1881, 1881);;
hat # add "Chester A. Arthur" (1881, 1885);;
hat # add "Grover Cleveland" (1885, 1889);;
hat # add "Benjamin Harrison" (1889, 1893);;
hat # add "Grover Cleveland" (1893, 1897);;
hat # add "William McKinley" (1897, 1901);;
hat # add "Theodore Roosevelt" (1901, 1909);;
hat # add "William Howard Taft" (1909, 1913);;
hat # add "Woodrow Wilson" (1913, 1921);;
hat # add "Warren G. Harding" (1921, 1923);;
hat # add "Calvin Coolidge" (1923, 1929);;
hat # add "Herbert Hoover" (1929, 1933);;
hat # add "Franklin D. Roosevelt" (1933, 1945);;
hat # add "Harry S. Truman" (1945, 1953);;
hat # add "Dwight D. Eisenhower" (1953, 1961);;
hat # add "John F. Kennedy" (1961, 1963);;
hat # add "Lyndon B. Johnson" (1963, 1969);;
hat # add "Richard Nixon" (1969, 1974);;
hat # add "Gerald Ford" (1874, 1977);;
hat # add "Jimmy Carter" (1977, 1981);;
hat # add "Ronald Reagan" (1981, 1989);;
hat # add "George Bush" (1989, 1993);;
hat # add "Bill Clinton" (1993, 2001);;
hat # add "George W. Bush" (2001, 2009);;
hat # add "Barack Obama" (2009, 2017);;
hat # add "Donald Trump" (2017, 2020);;

hat # shape;;
hat # find "Franklin Pierce";;
hat # find "George Washington";;
hat # find "Theodore Roosevelt";;

(* Set class *)

(* Set as the standard module *)
module IntPairs =
struct
  type t = int * int
  let compare (x0, y0) (x1,y1) =
    match Stdlib.compare x0 x1 with
      0 -> Stdlib.compare y0 y1
    | c -> c
end;;

module PairsSet = Set.Make(IntPairs);;

let myset = PairsSet.(empty |> add (1,2) |> add (1,3) |> add (3,2));;
let myset' = PairsSet.(empty |> add (1,2) |> add (1,3) |> add (2,3));;
PairsSet.is_empty myset;;
PairsSet.mem (1,2) myset;;
PairsSet.(is_empty (singleton (3,3)));;
PairsSet.elements myset;;
PairsSet.(elements (union myset myset'));;
PairsSet.(elements (inter myset myset'));;
PairsSet.(elements (diff myset myset'));;
PairsSet.(cardinal (union myset myset'));;

let print_pair (a, b) = Printf.printf "(%d, %d)\n" a b;;
print_pair (1,2);;

PairsSet.(iter print_pair (union myset myset'));;

(* object oriented set *)

module type SET =
sig
  type 'a tag (* abstract type of set *)
  class ['a] c : 
    object ('b)
      method is_empty : bool
      method mem : 'a -> bool
      method add : 'a -> 'b
      method union : 'b -> 'b
      method iter : ('a -> unit) -> unit
      method tag : 'a tag
    end
end;;

        
module Set : SET =
struct
  (* merge two ordered lists into a larger ordered list, 
     discarding duplication, and assuming that each list 
     itself does not contain duplication *)
  let rec merge l1 l2 =
    match l1 with
      [] -> l2
    | h1 :: t1 ->
      match l2 with
        [] -> l1
      | h2 :: t2 ->
        if h1 < h2 then h1 :: merge t1 l2
        else if h1 > h2 then h2 :: merge l1 t2
        else merge t1 l2

  type 'a tag = 'a list

  class ['a] c =
    object (_ : 'b)
      val repr = ([] : 'a list)
      method is_empty = (repr = [])
      method mem x = List.exists (( = ) x) repr
      method add x = {< repr = merge [x] repr >}
      method union (s : 'b) = {< repr = merge repr s#tag >}
      method tag = repr
      method iter (f : 'a -> unit) = List.iter f repr
    end
end;;

(* The scope of 'a is the entire structure/signature where it appeared *)

let myset = (((new Set.c) # add 1) # add 2) # add 3;;
let myset' = ((((new Set.c) # add 4) # add 5) # add 6) # add 1;;

myset #tag;;
myset' # iter print_int;;
(myset # union myset') #iter print_int;;

(* The subject/observer pattern *)

class virtual ['subject, 'event] observer =
  object
    method virtual notify : 'subject -> 'event -> unit
  end;;

(* The virtual observer class is essentially a parametric class
   where a virtual method is declared whose type contains the 
   two type parameters of the class. Therefore the class can be
   inherited in the following unintended contexts *)
class president =
  object
    inherit [string, int*int] observer
    method notify =
      fun name term -> print_string name;print_newline();
        print_pair term
  end;;

let p = new president;;      
p # notify "Grover Cleveland" (1885, 1889);;

class ['a, 'b] nothing =
  object
    inherit ['a, 'b] observer
    (* the parameters of observer are bound by 
       the "nothing" class's type parameters *)
    method notify = fun _ _ -> ()
  end;;

let n1 = new nothing;;
(* val n1 : ('_weak6, '_weak7) nothing = <obj> *)
n1 # notify;;
(* - : '_weak6 -> '_weak7 -> unit = <fun> *)
n1 # notify "a" "b";;
n1;; (* : (string, string) nothing *)
let n2 = new nothing;;
n2 # notify (object val a = 1 end)
  (object val b = 2 end);;
n2;; (* : (<  >, <  >) nothing *)

(* back to the subject/observer pattern topic *)

class ['observer, 'event] subject =
  object (self)
    val mutable observers = ([]: 'observer list)
    method add_observer obs = observers <- (obs :: observers)
    method notify_observers (e : 'event) =
      List.iter (fun x -> x#notify self e) observers
  end;;

(* We now define instances of the pattern by inheritance *)

type event = Raise | Resize | Move;;

let string_of_event = function
  Raise -> "Raise" | Resize -> "Resize" | Move -> "Move";;

let count = ref 0;;

class ['observer] window_subject =
  let id = count := succ !count; !count in
  object (self)
    inherit ['observer, event] subject
    val mutable position = 0
    method identity = id
    method move x = position <- position + x; self # notify_observers Move
    method draw = Printf.printf "{Position = %d}\n" position
  end;;

class ['subject] window_observer =
  object
    inherit ['subject, event] observer
    method notify s e = s # draw
  end;;

let window = new window_subject;;

let window_observer = new window_observer;;

window_observer # notify window Resize;;
window_observer;;
window # add_observer window_observer;;
window;;
window # move 1;;

(* By now the two objects: the window object and the window_observer
   object, still do not belong to two classes that serve as each other's 
   type parameter *)

class ['observer] richer_window_subject =
  object (self)
    inherit ['observer] window_subject
    val mutable size = 1
    method resize x = size <- size + x; self # notify_observers Resize
    val mutable top = false
    method raise = top <- true; self # notify_observers Raise
    method! draw = Printf.printf "{Position = %d; Size = %d}\n" position size
  end;;

class ['subject] richer_window_observer =
  object
    inherit ['subject] window_observer as super
    method notify s e = if e <> Raise then s # raise; super # notify s e
  end;;


let wd = new richer_window_subject and wdo = new richer_window_observer;;
wd # add_observer wdo;;
wd # resize  6;;

class ['subject] trace_observer =
  object
    inherit ['subject, event] observer
    method notify s e =
      Printf.printf
        "<Window %d <== %s>\n" s#identity (string_of_event e)
  end;;


(* several different kind of but compatible observers 
   can be added to the same window *)
let window = new richer_window_subject;;
window # add_observer (new richer_window_observer);;
window # add_observer (new trace_observer);;
window # move 1; window # resize 2;;
