{-# LANGUAGE BangPatterns #-}

fib :: (Num a, Eq a) => a -> a
fib n =
    if n == 0 || n == 1 then n
    else fib (n - 2) + fib (n - 1)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib2 n = fibs !! n

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' []     = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' []     = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) = e == x || elem' e xs

doubleAll :: Num t => [t] -> [t]
doubleAll []     = []
doubleAll (x:xs) = (2 * x) : doubleAll xs

squareAll :: Num t => [t] -> [t]
squareAll []     = []
squareAll (x:xs) = (x ^ 2) : squareAll xs

selectEven :: Integral t => [t] -> [t]
selectEven []   = []
selectEven (x:xs)
            | even x    = x : selectEven xs
            | otherwise = selectEven xs

avg :: Fractional a => [a] -> a
avg xs = sum xs / (fromIntegral $ length xs)

gAvg :: Floating a => [a] -> a
gAvg xs = (product xs) ** (1 / (fromIntegral $ length xs))

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
    where loop acc []     = acc
          loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
    where loop acc []     = acc
          loop acc (x:xs) = loop (x + acc) xs

prod'2 :: Num a => [a] -> a
prod'2 = helper 1
    where helper acc []     = acc
          helper acc (x:xs) = helper (acc * x) xs

length'2 :: [a] -> Int
length'2 = helper 0
    where helper acc []     = acc
          helper acc (x:xs) = helper (acc + 1) xs

sum'4 :: Num a => [a] -> a
sum'4 = loop 0
    where loop !acc []     = acc
          loop !acc (x:xs) = loop (x + acc) xs

sum'5 :: Num a => [a] -> a
sum'5 = loop 0
    where loop acc []     = acc
          loop acc (x:xs) = acc `seq` loop (x + acc) xs