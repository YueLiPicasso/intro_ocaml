module Var =
  struct
    type anchor = int list
    type env = int
    type scope = int
    type t =
      { anchor        : anchor
      ; env           : env
      ; scope         : scope
      ; index         : int
      ; mutable subst : Obj.t option
      ; constraints   : Obj.t list
      }
      
    let global_anchor = [-8]
                      
    (* The global anchor is not exposed by the module interface. The choice of [-8] as
       the global anchor is immaterial, as long as it is a non-empty list; actually, 
       the choice of "int list" as the type for anchors is immaterial as long as the type
       admits some value whose machine representation is a block. Possessing the reference 
       to this unique block is regarded as necessary for being a variable. Only the exported 
       function "make" can dispense such a secret reference. We may define instead:
       
       type anchor = unit option 
       
       let global_anchor = Some ()

     *)

    (* The "make" function creates fresh record values as variables 
       that have valid anchors. *)
                            
    let make ~env ~scope index = {
        anchor      = global_anchor
      ; env
      ; scope
      ; index
      ; subst       = None
      ; constraints = []
      }

    let dummy =
      let env = 0 and scope = 0 in make ~env ~scope 0
                                 
    let reify : ('a -> 'b) -> t -> int * 'b list =
      fun f {index; constraints} -> (index, List.map (fun x -> f (Obj.obj x)) constraints)

    (* What the "reify" function does? It takes a unary function f and a variable x, and returns 
       the index of x tupled with a list built from applying f to the constraint of x. *)     
  end
    
(* var_tag and var_size are the tag and size of the memory block that holds the record value
   "dummy". The tag is 0 and the size is 6 fields. *)
  
let var_tag, var_size =
  let dummy = Obj.repr Var.dummy in
  Obj.tag dummy, Obj.size dummy

(* polymorphic variable testing: taking a value x, if it is a variabe, wrap it by "Some"
   otherwise return "None". *)

let var : 'a -> Var.t option = fun x ->
  let x' = Obj.repr x in
  if  Obj.is_block x' && Obj.size x' = var_size && Obj.tag x' = var_tag &&
        let anchored : Obj.t -> bool =
          fun y -> Obj.field y 0 == Obj.repr Var.global_anchor
        in  anchored x'
  then Some ((Obj.obj x') : Var.t)
  else None
                          
(* If the global anchor is exposed, then the user can build fake variables like: 
   
   type fvar = Con of int list * int * int * int * unit option * unit list;;

   let myfv = Con(Var.global_anchor, 1,1,1,None,[]);;

   let myfv2 = Con(Var.global_anchor, 1,1,1, Some(),[();();()]);;

   The machine representation of "myfv" and "myfv2" would pass the "var" test. 
*)
