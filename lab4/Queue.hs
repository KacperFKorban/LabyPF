module Queue
    ( Queue
    , emptyQ
    , isEmptyQ -- :: Queue a -> Bool
    , addQ     -- :: a -> Queue a -> Queue a
    , remQ     -- :: Queue a -> (a, Queue a)
    ) where

newtype Queue a = Queue ([a], [a]) deriving Show

emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
addQ :: a -> Queue a -> Queue a
remQ :: Queue a -> (a, Queue a)

emptyQ = Queue ([], [])

isEmptyQ (Queue (xs, ys)) = null xs && null ys

addQ x (Queue (xs, ys)) = Queue (x:xs, ys)

remQ (Queue (xs, [])) = remQ $ Queue ([], reverse $ xs)
remQ (Queue (xs, y:ys)) = (y, Queue (xs, ys))
