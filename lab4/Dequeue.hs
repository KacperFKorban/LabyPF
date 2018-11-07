module Dequeue
    ( Dequeue
    , emptyDEQ
    , isEmptyDEQ
    , lengthDEQ
    , firstDEQ
    , lastDEQ
    , takeFrontDEQ
    , takeBackDEQ
    , pushFrontDEQ
    , popFrontDEQ
    , pushBackDEQ
    , popBackDEQ
    , fromListDEQ
    ) where

emptyDEQ :: Dequeue a
isEmptyDEQ :: Dequeue a -> Bool
lengthDEQ :: Dequeue a -> Int
firstDEQ :: Dequeue a -> Maybe a
lastDEQ :: Dequeue a -> Maybe a
takeFrontDEQ :: Int -> Dequeue a -> [a]
takeBackDEQ :: Int -> Dequeue a -> [a]
pushFrontDEQ :: Dequeue a -> a -> Dequeue a
popFrontDEQ :: Dequeue a -> Maybe (a, Dequeue a)
pushBackDEQ :: Dequeue a -> a -> Dequeue a
popBackDEQ :: Dequeue a -> Maybe (a, Dequeue a)
fromListDEQ :: [a] -> Dequeue a

data Dequeue a = Dequeue { front :: [a], lengthF :: Int, rear :: [a], lengthR :: Int } deriving Show

emptyDEQ = Dequeue [] 0 [] 0

isEmptyDEQ (Dequeue [] 0 [] 0) = True
isEmptyDEQ _                   = False

lengthDEQ (Dequeue _ lF _ lR) = lF + lR

headMay []     = Nothing
headMay (x:xs) = Just x

firstDEQ (Dequeue [] _ [x] _)  = Just x
firstDEQ (Dequeue front _ _ _) = headMay front

lastDEQ (Dequeue [x] _ [] _) = Just x
lastDEQ (Dequeue _ _ rear _) = headMay rear

takeFrontDEQ n (Dequeue front lengthF rear lengthR) = take n front ++ take (n - lengthF) (reverse rear)

takeBackDEQ n (Dequeue front lengthF rear lengthR) = take n rear ++ take (n - lengthR) (reverse front)

pushFrontDEQ (Dequeue front lengthF rear lengthR) x = check $ Dequeue (x : front) (lengthF + 1) rear lengthR

popFrontDEQ (Dequeue [] _ [] _)                     = Nothing
popFrontDEQ (Dequeue [] _ [x] _)                    = Just (x, emptyDEQ)
popFrontDEQ (Dequeue [] _ _ _ )                     = error "Something has gone terribly wrong"
popFrontDEQ (Dequeue (f : fs) lengthF rear lengthR) = Just (f, check $ Dequeue fs (lengthF - 1) rear lengthR)

pushBackDEQ (Dequeue front lengthF rear lengthR) x = check $ Dequeue front lengthF (x : rear) (lengthR + 1)

popBackDEQ (Dequeue [] _ [] _)                      = Nothing
popBackDEQ (Dequeue [x] _ [] _)                     = Just (x, emptyDEQ)
popBackDEQ (Dequeue _ _ [] _ )                      = error "Something has gone terribly wrong"
popBackDEQ (Dequeue front lengthF (r : rs) lengthR) = Just (r, check $ Dequeue front lengthF rs (lengthR - 1))

fromListDEQ = foldr (flip pushFrontDEQ) emptyDEQ

dqBalance :: Int
dqBalance = 2

check q@(Dequeue front lengthF rear lengthR)
    | lengthF > c * lengthR + 1 =
        let front' = take size1 front
            rear'  = rear ++ reverse (drop size1 front)
        in Dequeue front' size1 rear' size2
    | lengthR > c * lengthF + 1 =
        let front' = front ++ reverse (drop size1 rear)
            rear'  = take size1 rear
        in Dequeue front' size2 rear' size1
    | otherwise = q
    where
        size1 = (lengthF + lengthR) `div` 2
        size2 = lengthF + lengthR - size1
        c = dqBalance