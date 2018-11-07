fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

zad1 :: Integral a => [a] -> Bool
zad1 (x : y : _) | y `mod` x == 0 = True
zad1 _                            = False

zad2 :: Integral a => [a] -> Bool
zad2 (x : y : z : _) | z `mod` x == 0 = True
zad2 _                                = False