(* In the eyes of a lexer: 
kw -- keyword
id -- identifier

The example program has no literals, prefix/infix symbols, naming labels and line number directives.
Blanks and comments are omitted.
*)

module (* kw *) ARITH (* id *) = (* kw *) struct (* kw *)
  type (* kw *) nat (* id *)
    = (* kw *) Zero (* id *)
             | (* kw *) Succ (* id *) of (* kw *) nat (* id *)
  let (* kw *) rec (* kw *) twice (* id *) = (* kw *) function (* kw *)
      Zero (* id *) -> (* kw *) Zero (* id *)
    | (* kw *) Succ (* id *) x (* id *) -> (* kw *)
       Succ (* id *) ( (* kw *) Succ (* id *) ( (* kw *) twice (* id *) x (* id *) ) (* kw *) ) (* kw *)
end (* kw *) ;; (* kw *)

open (* kw *) ARITH (* id *) ;; (* kw *)

let (* kw *) two (* id *) = (* kw *) Succ (* id *) ( (* kw *) Succ (* id *) Zero (* id *) ) (* kw *) ;; (* kw *)

twice (* id *) two (* id *) ;; (* kw *)
