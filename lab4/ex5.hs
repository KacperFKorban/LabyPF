newtype MyInt = MkMyInt Int

instance Eq MyInt where
    MkMyInt i1 == MkMyInt i2 = i1 == i2

instance Ord MyInt where
    MkMyInt i1 <= MkMyInt i2 = i1 <= i2

instance Num MyInt where
    MkMyInt i1 + MkMyInt i2 = MkMyInt $ i1 + i2
    MkMyInt i1 - MkMyInt i2 = MkMyInt $ i1 - i2
    MkMyInt i1 * MkMyInt i2 = MkMyInt $ i1 * i2
    negate (MkMyInt i)      = MkMyInt $ negate i
    abs (MkMyInt i)         = MkMyInt $ abs i
    signum (MkMyInt i)      = MkMyInt $ signum i
    fromInteger int         = MkMyInt $ fromInteger int

instance Show MyInt where
    show (MkMyInt i) = "MkMyInt " ++ show i

data Fraction a = Fraction { num :: a, denom :: a }

instance Show a => Show (Fraction a) where
    show (Fraction n d) = show n ++ "/" ++ show d

instance (Num a, Eq a) => Eq (Fraction a) where
    Fraction n1 d1 == Fraction n2 d2 = n1 * d2 == n2 * d1

instance (Num a, Ord a) => Ord (Fraction a) where
    Fraction n1 d1 <= Fraction n2 d2 = n1 * d2 <= n2 * d1

instance Num a => Num (Fraction a) where
    Fraction n1 d1 + Fraction n2 d2 = Fraction (n1 * d2 + n2 * d1) (d1 * d2)
    Fraction n1 d1 - Fraction n2 d2 = Fraction (n1 * d2 - n2 * d1) (d1 * d2)
    Fraction n1 d1 * Fraction n2 d2 = Fraction (n1 * n2) (d1 * d2)
    negate (Fraction n d)           = Fraction (negate n) d
    abs (Fraction n d)              = Fraction (abs n) (abs d)
    signum (Fraction n d)           = Fraction (signum n) (signum d)
    fromInteger int                 = Fraction (fromInteger int) 1

data MyDouble = MyDouble Double deriving (Show, Eq, Ord)

instance Num MyDouble where
    MyDouble i1 + MyDouble i2 = MyDouble $ i1 + i2
    MyDouble i1 - MyDouble i2 = MyDouble $ i1 - i2
    MyDouble i1 * MyDouble i2 = MyDouble $ i1 * i2
    negate (MyDouble i)       = MyDouble $ negate i
    abs (MyDouble i)          = MyDouble $ abs i
    signum (MyDouble i)       = MyDouble $ signum i
    fromInteger int           = MyDouble $ fromInteger int

newtype MyList a = MyList [a]

instance Eq a => Eq (MyList a) where
    MyList [] == MyList []           = True
    MyList [] == _                   = False
    _ == MyList []                   = True
    MyList (a:as) == (MyList (b:bs)) = a == b && MyList as == MyList bs

instance (Eq a, Ord a) => Ord (MyList a) where
    MyList [] <= MyList []         = True
    MyList [] <= _                 = False
    _ <= MyList []                 = True
    MyList (a:as) <= MyList (b:bs) = a <= b && MyList as <= MyList bs
