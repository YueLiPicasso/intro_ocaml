module Stack.Data ( StkType, push, pop, empty ) where
    data StkType a = EmptyStk | Stk a (StkType a)
    push :: a -> StkType a -> StkType a
    push x s = Stk x s
    pop :: StkType a -> StkType a
    pop (Stk _ s) = s
    empty :: StkType a
    empty = EmptyStk
