(** exponential tuples: 
    An Expo-N tuple is a 2^(2^N) tuple; The Expo-O tuple is simply called the Expo tuple *)
open Logic;;


module Expo = struct
  @type 'a t = ('a,'a) LPair.t
   with show, gmap;;

  @type 'a ground = ('a,'a) LPair.ground
   with show, gmap;;

  @type 'a logic = ('a,'a) LPair.logic
   with show, gmap;;

  type ('a,'b) groundi = ('a,'b,'a,'b) LPair.groundi;;

  let reify : (VarEnv.t -> ('a,'b) injected -> 'b) -> VarEnv.t -> ('a,'b) groundi -> 'b logic
      = fun r h x -> LPair.reify r r h x;;
end;;
(*
module Expo1 = struct
   @type 'a t = 'a Expo.t Expo.t
   with show, gmap;;

  @type 'a ground = 'a Expo.ground Expo.ground
   with show, gmap;;

  @type 'a logic = 'a Expo.logic Expo.logic
   with show, gmap;;

  type ('a,'b) groundi =
      ('a ground, 'b logic) Expo.groundi;;

  let reify : (VarEnv.t -> ('a,'b) injected -> 'b) -> VarEnv.t -> ('a,'b) groundi -> 'b logic
      = fun r h x -> LPair.reify (LPair.reify r r) (LPair.reify r r) h x;;
end;;


module Expo2 = struct
  @type 'a t = 'a Expo1.t Expo1.t
   with show, gmap;;

  @type 'a ground = 'a Expo1.ground Expo1.ground
   with show, gmap;;

  @type 'a logic = 'a Expo1.logic Expo1.logic
   with show, gmap;;

  type ('a,'b) groundi =
      ('a ground, 'b logic) Expo1.groundi;;

  let reify : (VarEnv.t -> ('a,'b) injected -> 'b) -> VarEnv.t -> ('a,'b) groundi -> 'b logic
      = fun r h x -> Expo1.reify (Expo1.reify r) h x;;
end;;

*)
