module Stack.Type ( StkType, push, pop, empty ) where
  type StkType a = [a]
  push :: a -> StkType a -> StkType a
  push x xs = x : xs
  pop :: StkType a -> StkType a
  pop (_:xs) = xs
  empty :: StkType a
  empty = []

-- If we didn't export StkType,
-- then it is not in scope, and
-- the REPL command:
-- ghci> [1] :: StkType Int
-- would cause an error. But the type
-- synonym is still known to the type
-- checker, and the REPL command:
-- ghci> push 1 [2]
-- would pass successfully and gives
-- [1,2]
