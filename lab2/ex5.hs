zad1 = length [(x, y, z) | x <- [1..100], y <- [1..100], z <- [1..100], x + y > z, x + z > y, y + z > x]

isPrime :: Integral t => t -> Bool
isPrime 1 = False
isPrime n = [i | i <- [2 .. n-1], n `mod` i == 0] == []

numOfPrimesUpToTenThousand = length [x | x <- [1 .. 10000], isPrime x]

numOfPrimes n = length [x | x <- [1 .. n], isPrime x]

primes :: [Int]
primes = eratoSieve [2..]
    where
        eratoSieve :: [Int] -> [Int]
        eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

isPrime' :: Int -> Bool
isPrime' n = n `elem` (take n primes)

numOfPrimes' :: Int -> Int
numOfPrimes' n = length [x | x <- [1 .. n], isPrime' x]

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs