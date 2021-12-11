type t = Term.t Term.VarMap.t

let empty = Term.VarMap.empty           

type lterm = Var of Term.Var.t | Value of Term.t
          
let rec walk env subst x =
  walkv env subst x
and walkv env subst v =
  VarEnv.check_exn env v;
  match v.Term.Var.subst with
  | Some term -> walkt env subst (Obj.magic term)
  | None -> try walkt env subst (Term.VarMap.find v subst)
            with Not_found -> Var v
and walkt env subst t =
  match VarEnv.var env t with
  | Some v -> walkv env subst v
  | None -> Value t

                  
                  (* Term.Var.subst *)
                  (* Term.VarMap.find *)
