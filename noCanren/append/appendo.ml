open GT
open OCanren
open OCanren.Std
type ('a2, 'a1, 'a0) gexpr =
  | Con of 'a2 
  | Var of 'a1 
  | Arr of 'a1 * 'a0 
  | Brh of 'a0 * 'a0 * 'a0 
module For_gexpr =
  (Fmap3)(struct
            let rec fmap fa2 fa1 fa0 =
              function
              | Con a2 -> Con (fa2 a2)
              | Var a1 -> Var (fa1 a1)
              | Arr (a1_0, a0_1) -> Arr ((fa1 a1_0), (fa0 a0_1))
              | Brh (a0_0, a0_1, a0_2) -> Brh ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2))
            type ('a2, 'a1, 'a0) t = ('a2, 'a1, 'a0) gexpr
          end)
let rec con x__0 = inj (For_gexpr.distrib (Con x__0))
and var_ x__0 = inj (For_gexpr.distrib (Var x__0))
and arr x__0 x__1 = inj (For_gexpr.distrib (Arr (x__0, x__1)))
and brh x__0 x__1 x__2 = inj (For_gexpr.distrib (Brh (x__0, x__1, x__2)))
type ('a1, 'a0) gvalue =
  | Conv of 'a1 
  | Arrv of 'a0 
module For_gvalue = (Fmap2)(struct let rec fmap fa1 fa0 = function | Conv a1 -> Conv (fa1 a1) | Arrv a0 -> Arrv (fa0 a0)
                                   type ('a1, 'a0) t = ('a1, 'a0) gvalue end)
let rec conv x__0 = inj (For_gvalue.distrib (Conv x__0))
and arrv x__0 = inj (For_gvalue.distrib (Arrv x__0))
let rec assoc =
  fun key ->
    fun list ->
      fun q12 ->
        fresh (q13 q1 k v t q2 q6 q7) (q1 === ((pair k v) % t)) (k === q6) (
          q7 === q13) (key q13) (list q1) (conde [(q6 === q7) &&& (q2 === (!! true)); (q2 === (!! false)) &&& (q6 =/= q7)])
          (conde [(q2 === (!! true)) &&& (v === q12); (q2 === (!! false)) &&& (assoc (fun q14 -> q14 === q13) (fun q11 -> t === q11) q12)])
let rec nth  =
  fun arg1 ->
    fun arg2 ->
      fun q41 ->
        fresh (q16 q38 q39 l n q18 h t q19 q31 q32) (q16 === (pair q38 q39)) (
          q16 === (pair (arrv l) (conv n))) (l === q18) (q18 === (h % t)) (
          n === q31) (q32 === (!! 0)) (arg1 q38) (arg2 q39) (conde [(q31 === q32) &&& (q19 === (!! true)); (q19 === (!! false)) &&& (q31 =/= q32)])
          (conde
             [fresh (q26) (q19 === (!! true)) (q41 === (conv q26)) (h === q26);
             (q19 === (!! false)) &&&
               (nth (fun q22 -> fresh (q21) (q22 === (arrv q21)) (t === q21))
                  (fun q24 -> fresh (q23) (q24 === (conv q23)) ((-) (fun q37 -> n === q37) (fun q25 -> q25 === (!! 1)) q23)) q41)])
let rec eval_imp =
  fun s ->
    fun e ->
      fun q50 ->
        fresh (q51 q43) (s q51) (e q43)
          (conde
             [fresh (c q44) (q43 === (con c)) (q50 === (conv q44)) (c === q44);
             fresh (name) (q43 === (var_ name)) (assoc (fun q47 -> name === q47) (fun q52 -> q52 === q51) q50);
             fresh (name index) (q43 === (arr name index))
               (nth (assoc (fun q48 -> name === q48) (fun q52 -> q52 === q51)) (eval_imp (fun q52 -> q52 === q51) (fun q49 -> index === q49)) q50)])
