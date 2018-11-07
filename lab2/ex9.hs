qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
    where
        leftPart  xs = [ y | y <- xs, y <= x ]
        rightPart xs = [ y | y <- xs, y > x  ]

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
    where
        leftPart  xs = filter (<= x) xs
        rightPart xs = filter (> x) xs

mSort :: Ord a => [a] -> [a]
mSort []  = []
mSort [a] = [a]
mSort xs  = merge (mSort leftPart) (mSort rightPart)
    where n                     = length xs `div` 2
          (leftPart, rightPart) = splitAt n xs
          merge [] ys = ys
          merge xs [] = xs
          merge (x:xs) (y:ys)
                        | x < y = x : merge xs (y:ys)
                        | otherwise = y : merge (x:xs) ys

iSort :: Ord a => [a] -> [a]
iSort = helper []
    where helper acc []     = acc
          helper acc (x:xs) = acc `seq` helper (insert x [] acc) xs 
          insert x acc []   = acc ++ [x]
          insert x acc z@(y:ys)
                        | x <= y = acc ++ (x:z)
                        | otherwise = insert x (acc ++ [y]) ys

concat' :: [[a]] -> [a]
concat' xs = [x | ys <- xs, x <- ys]

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (x:xs) = x ++ concat'' xs

concat''' :: [[a]] -> [a]
concat''' = helper []
    where helper acc []     = acc
          helper acc (x:xs) = helper (acc ++ x) xs

isSorted :: [Int] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ []          = []
zip' [] _          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

unzip' :: [(a, b)] -> ([a],[b])
unzip' = helper [] []
    where helper acc1 acc2 [] = (acc1, acc2)
          helper acc1 acc2 ((x, y):xs) = helper (acc1 ++ [x]) (acc2 ++ [y]) xs

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' _ _ [] = []
zip3' _ [] _ = []
zip3' [] _ _ = []
zip3' (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3' xs ys zs

subList :: Eq a => [a] -> [a] -> Bool
subList [] [] = True
subList _ []  = False
subList p t@(x:xs)
        | length p > length t = False
        | isStartedWith p t   = True
        | otherwise           = subList p xs
        where isStartedWith [] _ = True
              isStartedWith _ [] = False
              isStartedWith (p:ps) (x:xs)
                        | p /= x    = False
                        | otherwise = isStartedWith ps xs