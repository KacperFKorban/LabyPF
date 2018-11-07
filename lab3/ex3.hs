sqr x = x^2

funcFactory n = case n of
        1 -> id
        2 -> sqr
        3 -> (^3)
        4 -> \x -> x^4
        5 -> intFunc
        _ -> const n
        where   intFunc x = x^5

fact 0 = 1
fact x = x * fact (x-1)

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n x = sum $ map (\k -> x ^ k / (fromIntegral $ fact k)) [0..n]

dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = \x -> (f (x + h) - f h) / h

dfc :: (Double -> Double) -> Double -> (Double -> Double)
dfc f h = \x -> (f (x + h) - f (x - h)) / 2 * h

d2f :: (Double -> Double) -> Double -> (Double -> Double)
d2f f h = dfc (dfc f h) h
