(<$<) :: (a -> b) -> a -> b
(<$<) = ($)
infixr 0 <$<  

(>$>) :: a -> (a -> b) -> b
x >$> f = f x
infixl 0 >$>

(<.<) :: (b -> c) -> (a -> b) -> (a -> c)
(<.<) = (.)
infixr 9 <.<

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
f >.> g = g . f
infixl 9 >.>

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

extractMaybe :: Maybe a -> a
extractMaybe Nothing = error "Can't extract from Nothing!"
extractMaybe (Just a) = a

insertMaybe :: a -> Maybe a
insertMaybe a = Just a

(>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  >^$> _ = Nothing
(Just x) >^$> f = f x
infixl 1 >^$>

f1 :: (Ord a, Num a) => a -> Maybe a
f1 x = if x > 0 then Just (x + 1) else Nothing

f2 :: (Eq a, Num a) => a -> Maybe a
f2 x = if x /= 0 then Just (10 * x) else Nothing

(>.>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
-- f >.>> g = \x -> f x >^$> g
f >.>> g = \x -> joinMaybe $ fmap g (f x)

joinMaybe :: Maybe (Maybe a) -> (Maybe a)
joinMaybe (Just x) = x
joinMaybe Nothing = Nothing

type MyWriterS a b = a -> (b, String)
