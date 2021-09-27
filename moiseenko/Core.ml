module Anchor : sig
  type t
     
  val anchor : t

  val is_valid : t -> bool
end = struct
  type t = int list
         
  let anchor = [1]

  let is_valid x = (x == anchor)
end

type env = int

module Var = struct
  type t =
    { anchor        : Anchor.t;
      env           : env;
      index         : int;
    }

  let idx { index } = index

  let make ~env index = { env; index; anchor = Anchor.anchor }

  let dummy = make ~env:0 0

end

exception Var_scope_violation of Var.t

module Term : sig
  type 'a t
     
  val var : env -> 'a t -> Var.t option

  val fresh : env -> 'a t

end = struct
  type 'a t = Obj.t

  let var_tag, var_size =
    let dummy = Obj.repr Var.dummy in
    Obj.tag dummy, Obj.size dummy

  let is_var env tx sx x =
    if (tx = var_tag) && (sx = var_size) then
      let v = (Obj.obj x : Var.t) in
      let a = v.Var.anchor in
      if (Obj.(is_block @@ repr a)) && (Anchor.is_valid a) then
        (if env = v.Var.env then true else raise (Var_scope_violation v))
      else false
    else false

  let is_box t =
    if (t <= Obj.last_non_constant_constructor_tag) &&
         (t >= Obj.first_non_constant_constructor_tag)
    then true
    else false

  let is_int = (=) Obj.int_tag
  let is_str = (=) Obj.string_tag

  let var env x =
    let x = Obj.repr x in
    let tx = Obj.tag x in
    if is_box tx then
      let sx = Obj.size x in
      if is_var env tx sx x then Some (Obj.magic x) else None
    else None

  let var_cnt = ref 0

  let fresh env =
    let idx = !var_cnt in
    var_cnt := 1 + !var_cnt;
    Obj.magic (Var.make ~env idx)

end

type 'a ilogic = 'a Term.t

external inj : 'a -> 'a ilogic = "%identity"

type 'a logic = Value of 'a | Var of Var.t

let observe : env -> 'a ilogic -> 'a logic = fun env t ->
  match Term.var env t with
  | None   -> Value (Obj.magic t)
  | Some v -> Var v

module Env = struct

  type 'a t = env -> 'a
            
  let return a = fun _ -> a

  let fmap f r env = f (r env)

  let bind r k env = k (r env) env

end

module State = struct

  type 'a t = env * 'a

  let extract (_, a) = a

  let extend (env, a) k = (env, k (env, a))

  let observe (env, a) = observe env a

end

exception Not_a_value

module Reifier = struct

  type ('a, 'b) t = ('a -> 'b) Env.t

  let reify = observe

  let prj k env t =
    match reify env t with
    | Value x -> x
    | Var v   -> k v

  (* can be implemented more efficiently, without allocation of `'a logic`,
   * but for demonstration purposes this implementation is okay
   *)
  let prj_exn env t =
    match reify env t with
    | Value x -> x
    | Var v   -> raise Not_a_value

  let apply r (env, a) = r env a

  let compose r r' env a = r' env (r env a)

  let fmap f r env a = f (r env a)

  let fcomap f r env a = r env (f a)

end

let fresh g env = g (Term.fresh env) env

let env_cnt = ref 0

let run rel =
  let env = !env_cnt in
  env_cnt := 1 + !env_cnt;
  (env, rel (Term.fresh env) env)
