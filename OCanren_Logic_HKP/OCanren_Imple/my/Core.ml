let unify = (===)

let (===) x y st =
  match State.unify x y st with
  | Some st -> success st
  | None -> failure st

module State = struct

  (* a record type for state *)
  type t =
    { env    : VarEnv.t
    ; subst  : VarSubst.t
    ; ctrs   : Disequality.t
    ; prunes : Prunes.t
    ; scope  : Term.Var.scope
    }

  (* create an empty state *)  
  let empty () =
    { env    = VarEnv.empty ()
    ; subst  = VarSubst.empty
    ; ctrs   = Disequality.empty
    ; prunes = Prunes.empty
    ; scope  = Term.Var.new_scope () }

  (* record field access *)
  let env {env} = env
  let subst {subst} = subst
  let constraints {ctrs} = ctrs
  let scope {scope} = scope
  let prunes {prunes} = prunes
    
  let unify x y ({env; subst; ctrs; scope} as st) =
    match VarSubst.unify ~scope env subst x y with
    | None -> None
    | Some (prefix, subst) ->
       match Disequality.recheck env subst ctrs prefix with
       | None -> None
       | Some ctrs ->
          match Prunes.recheck (prunes st) env subst with
          | Prunes.Violated -> None
          | Nonviolated -> Some {st with subst; ctrs}
          
  end

          
let success st = RStream.single st
let failure _ = RStream.nil                   

                  (* VarSubst.unify *)
                  (* prunes *)
                  (* Prunes.recheck *)
                  (* Disequality.recheck *)
                    (* State.unify *)
                    (* RStream.single *)
                    (* RStream.nil *)
              
