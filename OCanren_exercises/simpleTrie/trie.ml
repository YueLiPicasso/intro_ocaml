(** THe type of a particular trie *)
open ExpoTuples;;

@type t = {table : Register.t Expo2.t; alpha: Cell.t} with show, gmap;;

@type ground = {table : Register.ground Expo2.ground; alpha : Cell.ground} with show, gmap;;

@type logic = {table : Register.logic Expo2.logic; alpha : Cell.logic} with show, gmap;;

type groundi = {table : (Register.ground,Register.logic) Expo2.groundi; alpha : Cell.groundi};;

