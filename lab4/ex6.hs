class Mappable t where
    fMap :: (a -> b) -> t a -> t b
  
data Vec3D a = Vec3D { x::a, y::a, z::a } deriving Show
  
instance Mappable Vec3D where
    fMap f (Vec3D x y z) = Vec3D (f x) (f y) (f z)

newtype Pair a = Pair (a,a) deriving Show

instance Mappable Pair where
    fMap f (Pair (x,y)) = Pair (f x, f y)

data BinTree a = EmptyBT |
                NodeBT a (BinTree a) (BinTree a)
                deriving Show

instance Mappable BinTree where
    fMap _ EmptyBT = EmptyBT
    fMap f (NodeBT x l r) = NodeBT (f x) (fMap f l) (fMap f r)

instance Mappable Maybe where
    fMap _ Nothing  = Nothing
    fMap f (Just x) = Just $ f x

instance Mappable (Either a) where
    fMap _ (Left a) = Left a
    fMap f (Right a) = Right $ f a

instance Mappable ((->) a) where
    fMap f g = f . g
