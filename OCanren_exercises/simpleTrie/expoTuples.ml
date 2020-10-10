(** exponential tuples: An Expo-N tuple is a 2^(2^N) tuple*)
open Logic;;

(** Expo0 is shortly Expo: (_,_) *)
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

(** tuple of the form ((_,_),(_,_)) *)
module Expo1 = struct
   @type 'a t = 'a Expo.t Expo.t
   with show, gmap;;

  @type 'a ground = 'a Expo.ground Expo.ground
   with show, gmap;;

  @type 'a logic = 'a Expo.logic Expo.logic
   with show, gmap;;

  type ('a,'b) groundi =
      ('a Expo.ground, 'b Expo.logic) Expo.groundi;;

  let reify : (VarEnv.t -> ('a,'b) injected -> 'b) -> VarEnv.t -> ('a,'b) groundi -> 'b logic
      = fun r h x -> Expo.reify (Expo.reify r) h x;;
end;;

(** tuple of the form ((((_,_),(_,_)),((_,_),(_,_))),(((_,_),(_,_)),((_,_),(_,_)))) *)
module Expo2 = struct
  @type 'a t = 'a Expo1.t Expo1.t
   with show, gmap;;

  @type 'a ground = 'a Expo1.ground Expo1.ground
   with show, gmap;;

  @type 'a logic = 'a Expo1.logic Expo1.logic
   with show, gmap;;

  type ('a,'b) groundi =
      ('a Expo1.ground, 'b Expo1.logic) Expo1.groundi;;

  let reify : (VarEnv.t -> ('a,'b) injected -> 'b) -> VarEnv.t -> ('a,'b) groundi -> 'b logic
      = fun r h x -> Expo1.reify (Expo1.reify r) h x;;
end;;




  
