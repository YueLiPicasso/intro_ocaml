open More
open More.Core    
open Type

(* combinations of run, fresh, return *)
module Local_def : sig  
  val runrt : unit -> 'a ilogic State.t
  val runfr : unit -> 'a ilogic State.t  
end = struct
  let runrt = fun () -> run Env.return 
  let freshret = fun () -> fresh Env.return          
  let runfr = fun () -> run @@ fun _ ->  freshret ()
end

open Local_def
    
(* Reify variables *)

let _ = print_string @@ match Reifier.(apply reify @@ runrt ()) with
  | Value _ -> "failed\n"
  | Var _ -> "PASSED\n"
and _ = print_string @@ match Reifier.(apply reify @@ runfr ()) with
  | Value _ -> "failed\n"
  | Var _ -> "PASSED\n"
and _ = print_string @@
  match Reifier.(apply reify @@ run (fun _ -> Env.return @@ inj 44)) with
  | Value 44 -> "PASSED\n"
  | _ -> "failed\n"

let _ =
  let runaway : int ilogic ref = ref (Obj.magic ()) in  
  let state_ok = run (fun v -> runaway := v; Env.return v) in
  try Reifier.(apply reify @@ run (fun _ -> Env.return !runaway))   
               with
               | Var.Scope_violation _ -> print_string "PASSED\n";
                                          Reifier.(apply reify state_ok)
(* test fresh number generator *)
                                            
let _ = 
  print_string @@ Stdlib.List.fold_right
    (fun v s ->
       s ^ "(" ^
       string_of_int ((Obj.magic (Var.index v)) : int)
       ^
       " , "
       ^
       string_of_int ((Obj.magic (Var.env v))   : int)
       ^
       ")\n")
    Var.[fresh (fresh_env ());fresh (fresh_env ());
         fresh (fresh_env ());
         fresh (fresh_env ());fresh (fresh_env ());
         fresh (fresh_env ())]
    ""
(* [expr1 ; expr2; expr3 ; ... ; exprn] evalutation order 
   exprn , ..., expr3, expe2, expr1. *)

(* reify options *)

let _= print_string @@ 
  let v : 'a Core.logic Option.logic =
    Reifier.apply (Option.reify Reifier.reify) (runrt()) in
  match v with
  | Var _ -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let tm : 'a Core.logic Option.logic =
    Reifier.apply (Option.reify Reifier.reify)
      (run (fun v -> Env.return (inj (Some v))))
  in
  match tm with
  |  Value (Some (Var _)) -> "PASSED\n"
  | _ -> "failed\n"
    
let _ = print_string @@
  let tm : int Core.logic Option.logic =
    Reifier.apply (Option.reify Reifier.reify)
      (run (fun _ -> Env.return (inj (Some (inj 49)))))
  in
  match tm with
  |  Value (Some (Value 49)) -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let tm : bool Core.logic Option.logic Option.logic  =
    Reifier.apply (Option.reify (Option.reify Reifier.reify))
      (run (fun _ -> Env.return @@ inj (Some (inj (Some (inj true))))))
  in
  match tm with
  |  Value (Some (Value (Some (Value true)))) -> "PASSED\n"
  | _ -> "failed\n"


(* Reify List *)

let _ = print_string @@
  let tm = Reifier.Lazy.apply (List.reify Reifier.reify) (runrt()) in
  match tm with
  | Var _ -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let tm = Reifier.Lazy.apply (List.reify Reifier.reify)
      (run (fun v -> Env.return (inj List.(Cons(v, inj Nil))))) in
  match tm with
  | Value (Cons (Var _, Value Nil)) -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let tm = Reifier.Lazy.apply (List.reify Reifier.reify)
      (run (fun v -> Env.return
               (inj List.(Cons(v,
                               inj (Cons(v, (inj Nil)))))))) in
  match (tm : 'a Core.logic List.logic) with
  | Value(Cons(Var _, Value (Cons (Var _, Value Nil)))) -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let tm = Reifier.Lazy.apply (List.reify Reifier.reify)
      (run (fun v -> Env.return
               (inj List.(Cons(inj 5,
                               inj (Cons(inj 6, v))))))) in
  match (tm : int Core.logic List.logic) with
  | Value(Cons(Value 5, Value (Cons (Value 6, Var _)))) -> "PASSED\n"
  | _ -> "failed\n"

(* reify option list *)
    
let _ = print_string @@
  let tm = Reifier.Lazy.apply (List.reify (Option.reify Reifier.reify))
      (run (fun v -> Env.return
               (inj List.(Cons(inj (Some (inj 5)),
                               inj (Cons(v, (inj Nil)))))))) in
  match (tm : int Core.logic Option.logic List.logic) with
  | Value(Cons(Value(Some(Value 5)), Value (Cons (Var _, Value Nil))))
    -> "PASSED\n"
  | _ -> "failed\n"


let _ = print_string @@
  let tm = Reifier.Lazy.apply (List.reify (Option.reify Reifier.reify))
      (run (fun v -> Env.return
               (inj List.(Cons(inj (Some v),
                               inj (Cons(inj None, (inj Nil)))))))) in
  match (tm : int Core.logic Option.logic List.logic) with
  | Value(Cons(Value(Some(Var _)), Value (Cons (Value None, Value Nil))))
    -> "PASSED\n"
  | _ -> "failed\n"

(* reifier not deep enough in general but deep enough for the special case *)

let _ = print_string @@
  let (tm : Option.Nested.ilogic Option.logic Option.logic List.logic)
    = Reifier.Lazy.apply (List.reify (Option.reify Reifier.reify))
      ((run (fun v -> Env.return
                (inj List.(Cons(inj (Some v),
                                inj (Cons(v, (inj Nil))))))))
       :  Option.Nested.ilogic List.ilogic State.t) in
  match tm with
  | Value(Cons(Value(Some(Var _)), Value (Cons (Var _, Value Nil))))
    -> "PASSED\n"
  | _ -> "failed\n"

(* partial reification using a not-deep-enough reifier *)

let _ = print_string @@
  let (tm : (Option.Nested.ilogic Option.logic Option.logic List.logic)) =
    Reifier.Lazy.apply (List.reify (Option.reify Reifier.reify))
      ((run (fun v -> Env.return
               (inj List.(Cons(inj (Some (inj (Some (inj (Some v))))),
                               inj (Cons(v, (inj Nil))))))))
    :  Option.Nested.ilogic List.ilogic State.t) in
  match tm with
  | Value(Cons(Value(Some(Value(Some _))), Value (Cons (Var _, Value Nil))))
    -> "PASSED\n"
  | _ -> "failed\n"

(* How to get two vars of different but correct type? *)

let _ = print_string @@
  match Reifier.Lazy.apply (List.reify Reifier.reify)
          (run (fun x -> fresh (fun y -> Env.return @@ List.cons x y))) with
  | Value(Cons(Var v1, Var v2))
    when Var.(index v1 <> index v2) && Var.(env v1 = env v2) -> "PASSED\n"
  | _ -> "failed"

let _ = print_string @@
  match Reifier.Lazy.apply
          (List.reify
             (Option.reify
                (Reifier.reify : (int Core.ilogic, int Core.logic) Reifier.t)))
          (run (fun x ->
               fresh (fun y ->
                   Env.return
                     List.(cons (Option.some y)
                             (cons x (nil()))))))
  with
  | Value(Cons(Value(Some(Var v1)), Value(Cons(Var v2, Value Nil))))
    when Var.(index v1 <> index v2) && Var.(env v1 = env v2) -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  match Reifier.Lazy.apply
          (List.reify
             (Option.reify
                (Reifier.reify : (int Core.ilogic, int Core.logic) Reifier.t)))
          (run (fun v1 ->
              fresh (fun v2 ->
                fresh (fun v3 ->
                  Env.return
                    List.(cons v1
                            (cons (Option.some v2)
                               (cons (Option.none()) v3)))))))
  with
  | Value(Cons(Var v1,
               Value(Cons(Value(Some(Var v2)),
                          Value(Cons(Value None, Var v3))))))
    when Var.(index v1 < index v2) && Var.(index v2 < index v3) -> "PASSED\n"
  | _ -> "failed\n"
    
(* reify infinitely nested options *)

let _ = print_string @@
  let (tm : Option.Nested.logic list)
    = Stdlib.List.map (Reifier.Lazy.apply Option.Nested.reify)
      ([(run Env.return);
        (run (fun _ -> Env.return Option.(none())));
        (run (fun _ -> Env.return Option.(some (none()))));
        (run (fun _ -> Env.return Option.(some (some (none())))));
        (run (fun _ -> Env.return Option.(some (some (some (none()))))));
        (run (fun _ -> Env.return Option.(some (some (some (some (none())))))))] 
       :  Option.Nested.ilogic State.t list ) in
  match tm with
  | [Var _ ;
     Value None;
     Value(Some(Value None));
     Value(Some(Value(Some(Value None))));
     Value(Some(Value(Some(Value(Some(Value None))))));
     Value(Some(Value(Some(Value(Some(Value(Some(Value None))))))))]
    -> "PASSED\n"
  | _ -> "failed\n"

(* providing to a lazy reifier a lazy sub-refier *)

let cons = List.cons and nil = List.nil and some = Option.some and none = Option.none

let _ =
  print_string @@
  let (tm : (Option.Nested.logic List.logic))
    = Reifier.Lazy.apply (List.reify' Option.Nested.reify)
      ((run (fun v -> Env.return
                (cons (some v) (cons v (cons (some (some (some v))) (nil()))))))
       :  Option.Nested.ilogic List.ilogic State.t) in
  match tm with
  | Value
      (Cons(Value(Some(Var _)),
            Value (Cons (Var _,
                         Value(Cons(Value(Some(Value(Some(Value(Some(Var _)))))),
                                    Value Nil))))))
    -> "PASSED\n"
  | _ -> "failed\n"

(*[[Some None];
   [Some(Some(Some None)); Some(Some v)]] *)
let _ =
  print_string @@
  let (tm : Option.Nested.logic List.logic List.logic) =
    Reifier.Lazy.apply (List.reify' (List.reify' Option.Nested.reify))
      (run (fun v -> Env.return (cons (cons (some (none())) (nil()))
                                   (cons (cons (some(some(some (none()))))
                                            (cons (some(some v)) (nil()))) (nil())))))
in
match tm with
| Value(Cons(Value(Cons(Value(Some(Value None)),Value Nil)),
             Value(Cons(Value(Cons(Value(Some(Value(Some(Value(Some(Value None)))))),
                                   Value(Cons(Value(Some(Value(Some(Var _)))),Value Nil)))),
                        Value(Nil)))))
     -> "PASSED\n"
  | _ -> "failed\n"
