concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

concat'' :: [[a]] -> [a]
concat'' xs = [x | y <- xs, x <- y]

concat''' :: [[a]] -> [a]
concat''' = foldr (++) []
