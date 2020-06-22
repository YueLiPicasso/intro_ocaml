open GT
open OCanren
open OCanren.Std


@type rod = A | B | C with show 

@type move = ocanren { rod * rod } with show 
