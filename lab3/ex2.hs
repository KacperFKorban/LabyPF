sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' []      = 0
sumSqr' (x:xs)  = x ^ 2 + sumSqr' xs

sumWith :: Fractional a => (a -> a) -> [a] -> a
sumWith _ []        = 0
sumWith f (x:xs)    = f x + sumWith f xs

sum'' = sumWith id

sumSqr = sumWith (^2)

sumCube = sumWith (^3)

sumAbs = sumWith abs

listLength = sumWith (const 1)

prod' :: Num a => [a] -> a
prod' []        = 1
prod' (x:xs)    = x * prod' xs

prodWith :: Fractional a => (a -> a) -> [a] -> a
prodWith _ []       = 1
prodWith f (x:xs)   = f x * prodWith f xs

prod = prodWith id

prodSqr = prodWith (^2)

prodCube = prodWith (^3)

prodAbs = prodWith abs

foldWith :: Fractional a => (a -> a) -> (a -> a -> a) -> a -> [a] -> a
foldWith _ _ e []       = e
foldWith f g e (x:xs)   = g (f x) (foldWith f g e xs)
