open More
open More.Core

module type T1 = sig
  type 'a t
  type 'a logic = 'a t Core.logic
  type 'a ilogic = 'a t Core.ilogic
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

module type T2 = sig
  type ('a, 'b) t
  type ('a, 'b) logic  = ('a, 'b) t Core.logic
  type ('a, 'b) ilogic = ('a, 'b) t Core.ilogic
  val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
end


module Fmap1 (T : T1) = struct
  
  let reify = fun ra () ->
    Reifier.reify >>= (fun r -> (ra >>= (fun fa ->
        Env.return (fun x ->
            match r () x with
            | Var _ as v   -> v
            | Value t -> Value (T.fmap (fa()) t)))))

  module Lazy = struct
    
    let reify = fun ra () -> 
      Reifier.reify >>= (fun r -> ra >>= (fun fa ->
          Env.return (fun x -> match r () @@ x () with
              | Var _ as v -> fun () -> v
              | Value t -> fun () -> Value (T.fmap (fa()) t))))
                        
  end
end

module Fmap2 (T : T2) = struct
  
  let reify = fun ra rb () -> 
    Reifier.reify >>= (fun r -> ra >>= (fun fa -> rb >>= (fun fb ->
        Env.return (fun x -> match r () x with
            | Var _ as v -> v
            | Value t -> Value (T.fmap (fa())  (fb()) t)))))
      
  module Lazy = struct
    
    let reify = fun ra rb () -> 
      Reifier.reify >>= (fun r -> ra >>= (fun fa -> rb >>= (fun fb ->
          Env.return (fun x -> match r () @@ x () with
              | Var _ as v -> fun () -> v
              | Value t -> fun () -> Value (T.fmap (fa()) (fb()) t)))))
        
  end
end
