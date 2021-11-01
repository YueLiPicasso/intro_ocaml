open OCanren

(* higher kinded polymorphsm (hkp) support *)

type ('a, 't) app

(* OCanren logic term infrastructure *)
   
external distrib : (('a,'b) injected, 't) app -> (('a, 't) app, ('b,'t) app) injected
  = "%identity"
external distrib2 : (('a,'b) injected, (('c, 'd) injected, 't) app) app
                    -> (('a, ('c,'t) app) app, ('b, ('d, 't) app) app) injected
  = "%identity"  

(* user types *)

module LoList = struct
  (* OCanren types *)
  
  @type ('a, 'b) t = Nil | Cons of 'a * 'b with show
  @type 'a g = ('a, 'a g) t with show
  type 'a l = ('a, 'a l) t logic 
  type ('a, 'b) o = ('a g, 'b l) injected

  (* hkp features *)

  (* the brand for t *)                
  type b 

  (* to app form *)   
  external toap : ('a1, 'a2) t -> ('a1, ('a2, b) app) app = "%identity"

  (* from app form *)                                                        
  external frap : (('a1, ('a2, b) app) app, ('a3, ('a4, b) app) app ) injected
                  -> (('a1,'a2) t, ('a3,'a4) t) injected = "%identity"

  (* OCanren value constructor representation *)
                                                         
  let nil () = inj @@ frap @@ distrib2 @@ toap Nil
  let cons x y = inj @@ frap @@ distrib2 @@ toap @@ Cons(x, y)

  (* relation def. *)
               
  let rec appendo a b ab =
  conde [
    (a === nil ()) &&& (b === ab);
    Fresh.three (fun h t ab' ->
      (a === cons h t) &&&
      (cons h ab' === ab) &&&
        (appendo t b ab'))]
end


@type nl = Std.Nat.ground LoList.g with show;;              

let _ =  let open Std.Nat in let open LoList in
    Stdlib.List.iter (fun (a, b) -> print_endline (GT.show(nl) a ^ ", " ^ GT.show(nl) b)) @@
      Stream.take ~n:4 @@ run qr
      (fun q r -> LoList.appendo q r (cons zero (cons one (cons zero (nil())))))
      (fun q r -> q#prj, r#prj)
