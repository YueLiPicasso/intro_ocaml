type 'a t = Obj.t            

(* the calling context of this function is that x is a memory object 
   and its size and tag are known to be sx and tx resp. and tx is known 
   to be a box tag *)
  
let is_var env tx sx x =
  if (tx = Var.tag) && (sx = Var.size) then
    let v = (Obj.obj x : Var.t) in
    let a = Var.anchor v in
    if (Obj.(is_block @@ repr a)) && (Anchor.is a) then
      (if env = Var.env v then true else raise (Var.Scope_violation v))
    else false
  else false
  
let is_box t =
  if (t <= Obj.last_non_constant_constructor_tag) &&
       (t >= Obj.first_non_constant_constructor_tag)
  then true
  else false

let var env x =
  let x = Obj.repr x in
  let tx = Obj.tag x in
  if is_box tx then
    let sx = Obj.size x in
    if is_var env tx sx x then Some (Obj.magic x) else None
  else None

let fresh env = Obj.repr (Var.fresh env)
