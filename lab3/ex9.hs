sumWith g []     = 0
sumWith g (x:xs) = g x + sumWith g xs

prodWith g []     = 1
prodWith g (x:xs) = g x * prodWith g xs

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
    where
        go acc g []     = acc
        go acc g (x:xs) = go (g x + acc) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
    where
        go acc g []     = acc
        go acc g (x:xs) = go (g x * acc) g xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x $ foldr' f z xs

sumWith'' g = foldr' (\x acc -> g x + acc) 0

prodWith'' g = foldr' (\x acc -> g x * acc) 1

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs

sumWith''' g = foldl' (\acc x -> g x + acc) 0

prodWith''' g = foldl' (\acc x -> g x * acc) 1

-- błąd w 9.3.5 zamiast foldl1 (-) 0 list321 powinno być foldl1 (-) list321

map' f = foldr (\x acc -> f x : acc) []

map'' f = foldl (\acc x -> acc ++ [f x]) []

filter' p = foldr (\x acc -> if p x then x : acc else acc) []

filter'' p = foldl (\acc x -> if p x then acc ++ [x] else acc) []

foldl'' f z xs = foldr (\x g a -> g $ f a x) id xs z

foldr'' f z xs = foldl (\g x a -> g $ f x a) id xs z
