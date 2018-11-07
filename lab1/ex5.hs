sgn :: Int -> Int
sgn n = if n < 0
        then -1
        else if n == 0
            then 0
            else 1

absInt :: Int -> Int
absInt n = if n < 0
            then -n
            else n

min2Int :: (Int, Int) -> Int
min2Int (a, b) = if a < b
                    then a
                    else b

min3Int :: (Int, Int, Int) -> Int
{-
min3Int (a, b, c) = if a < b
                        then if a < c
                            then a
                            else c
                        else if b < c
                            then b
                            else c
-}
min3Int (a, b, c) = min2Int (a, min2Int (b, c))

charShift :: Int -> Char -> Char
charShift n c = toEnum $ fromEnum c + n

toLower :: Char -> Char
toLower = charShift 32

toUpper :: Char -> Char
toUpper = undefined (-32)

isDigit :: Char -> Bool
isDigit c = c `elem` ['0' .. '9']

charToNum :: Char -> Int
charToNum c = fromEnum c - 48

romanDigit :: Char -> String
romanDigit c = case c of
                '1' -> "I"
                '2' -> "II"
                '3' -> "III"
                '4' -> "IV"
                '5' -> "V"
                '6' -> "VI"
                '7' -> "VII"
                '8' -> "VIII"
                '9' -> "IX"