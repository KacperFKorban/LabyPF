import Data.List (sort, reverse)

sortDesc :: Ord a => [a] -> [a]
sortDesc = reverse . sort

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt f g = all (uncurry (==)) . map (\x -> (f x, g x))

infixl 9 >.>
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f . g

composeFunList :: [a -> a] -> a -> a
composeFunList = foldr (.) id

addAndTimesTwo = ((.).(.)) (*2) (+)
