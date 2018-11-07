isPalindrome :: [Char] -> Bool
isPalindrome x = x == reverse x

getElemAtIdx :: Int -> [a] -> a
getElemAtIdx n = head . drop n

toUpper :: Char -> Char
toUpper c = toEnum $ fromEnum c - 32

capitalize :: [Char] -> [Char]
capitalize w = (toUpper $ head w) : tail w