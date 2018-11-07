not' :: Bool -> Bool
not' True   = False
not' False  = True

isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True
isItTheAnswer _      = False

or' :: (Bool, Bool) -> Bool
or' (False, False) = False
or' _              = True

and' :: (Bool, Bool) -> Bool
and' (True, True) = True
and' _            = False

nand' :: (Bool, Bool) -> Bool
nand' = not' . and'

xor' :: (Bool, Bool) -> Bool
xor' (False, True) = True
xor' (True, False) = True
xor' _             = False