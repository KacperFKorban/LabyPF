onlyEven [] = []
onlyEven (x:xs)
    | even x    = x : onlyEven xs
    | otherwise = onlyEven xs

onlyOdd [] = []
onlyOdd (x:xs)
    | odd x     = x : onlyOdd xs
    | otherwise = onlyOdd xs

onlyUpper [] = []
onlyUpper (x:xs)
    | x `elem` ['A'..'Z']   = x : onlyUpper xs
    | otherwise             = onlyUpper xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

onlyEven' = filter' even

onlyOdd' = filter odd

onlyUpper' = filter' (`elem` ['A'..'Z'])

-- length $ onlyEven [1..10^6]
-- length $ filter even [1..10^6]

-- length [x | x <- [1..10^6], even x]
