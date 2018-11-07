data CartInt2DVec = MkCartInt2DVec Int Int

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec a = MkCart2DVec { x :: a, y :: a }

data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

data Cart3DVec a = Cart3DVec a a a deriving Ord

xCoord3D :: Cart3DVec a -> a
xCoord3D (Cart3DVec x _ _) = x

yCoord3D :: Cart3DVec a -> a
yCoord3D (Cart3DVec _ y _) = y

zCoord3D :: Cart3DVec a -> a
zCoord3D (Cart3DVec _ _ z) = z

data Cart3DVec' a = Cart3DVec' { _x :: a, _y :: a, _z :: a }

data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r)      = pi * r ^ 2
area (Rectangle a b) = a * b

data Tree a = EmptyT |
              Node a (Tree a) (Tree a)
              deriving Show

rootValue :: Tree a -> a
rootValue EmptyT       = error "rootValue: the empty tree has no root!"
rootValue (Node x _ _) = x

data TrafficLights = RedL |
                     OrangeL |
                     GreenL
                     deriving (Show, Eq)

data DriverAction = Stop |
                    Prepare |
                    Drive
                    deriving (Show, Eq)

actionFor :: TrafficLights -> DriverAction
actionFor RedL    = Stop
actionFor OrangeL = Prepare
actionFor GreenL  = Drive

instance Eq a => Eq (Cart3DVec a) where
    (Cart3DVec a1 b1 c1) == (Cart3DVec a2 b2 c2) = a1 == a2 && b1 == b2 && c1 == c2

instance Num a => Num (Cart3DVec a) where
    (Cart3DVec a1 b1 c1) + (Cart3DVec a2 b2 c2) = Cart3DVec (a1 + a2) (b1 + b2) (c1 + c2)
    (Cart3DVec a1 b1 c1) * (Cart3DVec a2 b2 c2) = Cart3DVec (b1 * c2 - c1 * b2) (c1 * a2 - a1 * c2) (a1 * b2 - b1 * a2)
    abs (Cart3DVec a b c)                       = Cart3DVec (abs a) (abs b) (abs c)
    signum (Cart3DVec a b c)                    = Cart3DVec a b c
    fromInteger int                             = Cart3DVec (fromInteger int) 0 0
    negate (Cart3DVec a b c)                    = Cart3DVec (negate a) (negate b) (negate c)
{-
instance Ord a => Ord (Cart3DVec a) where
    a <= b = if xCoord3D a <= xCoord3D b
                then True
                else if yCoord3D a <= yCoord3D b
                    then True
                    else zCoord3D a <= zCoord3D b
-}

class VectorLike t where
    (|==|) :: Eq a => t a -> t a -> Bool
    (|+|), (|-|) :: (Num a) => t a -> t a -> t a
    (|*|) :: (Num a) => t a -> t a -> a
    (||?), (|-?) :: (Num a, Eq a) => t a -> t a -> Bool
    vectLength :: Floating a => t a -> a
    unitVectOf :: Floating a => t a -> t a

instance VectorLike Cart2DVec where
    MkCart2DVec x1 y1 |==| MkCart2DVec x2 y2 = x1 == x2 && y1 == y2
    MkCart2DVec x1 y1 |+| MkCart2DVec x2 y2  = MkCart2DVec (x1 + x2) (y1 + y2)
    MkCart2DVec x1 y1 |-| MkCart2DVec x2 y2  = MkCart2DVec (x1 - x2) (y1 - y2)
    MkCart2DVec x1 y1 |*| MkCart2DVec x2 y2  = x1 * x2 + y1 * y2
    MkCart2DVec x1 y1 ||? MkCart2DVec x2 y2  = x1 == x2 || x1 == (-x2)
    MkCart2DVec x1 y1 |-? MkCart2DVec x2 y2  = x1 == ((^(-1)) $ x2) || x1 == ((^(-1)) $ (-x2))
    vectLength (MkCart2DVec x y)             = sqrt (x ^ 2 + y ^ 2)
    unitVectOf (MkCart2DVec x y)             = MkCart2DVec (x / l) (y / l)
        where l = vectLength $ MkCart2DVec x y

instance VectorLike Cart3DVec where
    Cart3DVec x1 y1 z1 |==| Cart3DVec x2 y2 z2 = x1 == x2 && y1 == y2 && z1 == z2
    Cart3DVec x1 y1 z1 |+| Cart3DVec x2 y2 z2  = Cart3DVec (x1 + x2) (y1 + y2) (z1 + z2)
    Cart3DVec x1 y1 z1 |-| Cart3DVec x2 y2 z2  = Cart3DVec (x1 - x2) (y1 - y2) (z1 - z2)
    Cart3DVec x1 y1 z1 |*| Cart3DVec x2 y2 z2  = x1 * x2 + y1 * y2 + z1 * z2
    Cart3DVec x1 y1 z1 ||? Cart3DVec x2 y2 z2  = Cart3DVec (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2) |==| Cart3DVec 0 0 0
    v1 |-? v2                                  = v1 |*| v2 == 0
    vectLength (Cart3DVec x y z)               = sqrt (x ^ 2 + sqrt (y ^ 2 + z ^ 2))
    unitVectOf (Cart3DVec x y z)               = Cart3DVec (x / l) (y / l) (z / l)
        where l = vectLength $ Cart3DVec x y z
