module Stack.Newtype ( push, pop, empty ) where
     newtype StkType a = Stk [a]
     push :: a -> StkType a -> StkType a
     push x (Stk s) = Stk (x:s)
     pop :: StkType a -> StkType a
     pop (Stk (_:s)) = Stk s
     empty :: StkType a
     empty = Stk []
