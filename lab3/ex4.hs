funcList :: [ Double -> Double ]
funcList = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x ]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)

funcListExt = (\x -> sqrt (1 + x)) : funcList

dfc :: (Double -> Double) -> Double -> (Double -> Double)
dfc f h = \x -> (f (x + h) - f (x - h)) / 2 * h

d2f :: (Double -> Double) -> Double -> (Double -> Double)
d2f f h = dfc (dfc f h) h

--Nie wiem co mam zrobić w 4.3.2 :(