isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = all (== True) $ zipWith (<=) (init xs) (tail xs)

everySecond :: [t] -> [t]
everySecond [] = []
everySecond [x] = [x]
everySecond (x:y:xs) = x : everySecond xs

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] _ _               = []
zip3' _ [] _               = []
zip3' _ _ []               = []
zip3' (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3' xs ys zs

unzip3' :: [(a, b, c)] -> ([a], [b], [c])
unzip3' = helper [] [] []
    where
        helper acc1 acc2 acc3 [] = (acc1, acc2, acc3)
        helper acc1 acc2 acc3 ((x, y, z):rest) = helper (acc1 ++ [x]) (acc2 ++ [y]) (acc3 ++ [z]) rest

isSortedDesc :: Ord a => [a] -> Bool
isSortedDesc xs = all (== True) $ zipWith (>=) (init xs) (tail xs)

isSorted :: Ord a => [a] -> Bool
isSorted = (||) <$> isSortedAsc <*> isSortedDesc
