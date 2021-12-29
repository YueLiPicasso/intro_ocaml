type 'a logic = Value of 'a | Var of Var.t

type 'a ilogic = 'a Term.t
        
external inj : 'a -> 'a ilogic = "%identity"
                             
let observe : Var.env -> 'a ilogic -> 'a logic =
  fun env t -> match Term.var env t with
               | None -> Value (Obj.magic t)
               | Some v -> Var v

module Env = struct
  type 'a t = Var.env -> 'a 
  let return a     = fun _ -> a
  let fmap f r env = f (r env)
  let bind r k env = k (r env) env      
  module Lazy = struct
    let bind r k env = k (Lazy.force r env) env
  end
end

module State = struct
  type 'a t = Var.env * 'a
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
    | Var v -> k v
  let prj_exn env t =
    match reify env t with
    | Value x -> x
    | Var _ -> raise Not_a_value
  let apply r (env, a) = r env a
  let compose r r' env a = r' env (r env a)
  let fmap f r env a = f (r env a)
  let fcomap f r env a = r env (f a)
end
                   
let fresh g env = g (Term.fresh env) env

let run rel = let env = Var.fresh_env () in
              (env, rel (Term.fresh env) env)



