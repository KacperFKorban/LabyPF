fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^)

_ToPower5 :: Num a => a -> a
_ToPower5 = (^ 5)

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = (5 -)

subtr5From_ :: Num a => a -> a
subtr5From_ x = x - 5

flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f b a = f a b

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f c b a = f a b c