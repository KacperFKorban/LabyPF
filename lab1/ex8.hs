not' :: Bool -> Bool
not' a = case a of
            True -> False
            False -> True

absInt n = case (n >= 0) of
            True    -> n
            _       -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer n = case n of
                    "Love"  -> True
                    _       -> False

or' :: (Bool, Bool) -> Bool
or' pr = case pr of
                (False, False)  -> False
                _               -> True

and' :: (Bool, Bool) -> Bool
and' pr = case pr of
            (True, True)    -> True
            _               -> False

nand' :: (Bool, Bool) -> Bool
nand' = not' . and'

xor' :: (Bool, Bool) -> Bool
xor' pr = case pr of
            (True, False)   -> True
            (False, True)   -> True
            _               -> False