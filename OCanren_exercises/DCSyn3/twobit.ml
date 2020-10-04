(** short names for two-bit binaries *)

open OCanren;;
open OCanren.Std;;
open Coar;;

let c0  : Constnt2.groundi = ocanren { (b0,b0) };;
let c1  : Constnt2.groundi = ocanren { (b0,b1) };;
let c2  : Constnt2.groundi = ocanren { (b1,b0) };;
let c3  : Constnt2.groundi = ocanren { (b1,b1) };;
