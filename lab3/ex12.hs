import Data.Char
import Data.List

capitalize :: [Char] -> [Char]
capitalize []     = []
capitalize (x:xs) = toUpper x : (map toLower xs)

formatStr s = unwords .
            map capitalize .
            filter ((>1) . length) $
            words s

prodPrices p = case p of
    "A" -> 100
    "B" -> 500
    "C" -> 1000
    _   -> error "Unknown product"

products = ["A","B","C"]

discStr1 p
    | price > 999 = 0.3 * price
    | otherwise   = 0.1 * price
    where price = prodPrices p

discStr2 p = 0.2 * prodPrices p

totalDiscout discStr =
    sum .
    map discStr .
    filter ((>499) . prodPrices)
