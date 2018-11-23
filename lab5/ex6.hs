{-# LANGUAGE DeriveFunctor #-}

newtype Box a = MkBox a deriving (Show, Functor)

data MyList a = EmptyList
              | Cons a (MyList a) deriving (Show, Functor)

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)

instance Functor BinTree where
  fmap _ EmptyBT        = EmptyBT
  fmap f (NodeBT a l r) = NodeBT (f a) (fmap f l) (fmap f r)

newtype Pair b a = Pair { getPair :: (a,b) }

data Tree2 a = EmptyT2 | Leaf a | Node (Tree2 a) a (Tree2 a) deriving Show

data GTree a = GLeaf a | GNode [GTree a] deriving Show

instance Functor (Pair b) where
  fmap f (Pair (a, b)) = Pair (f a, b)

instance Functor Tree2 where
  fmap _ EmptyT2      = EmptyT2
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Functor GTree where
  fmap f (GLeaf a) = GLeaf (f a)
  fmap f (GNode bs) = GNode (fmap (fmap f) bs)

{-
instance Functor ((->) a) where
  fmap f g = f . g
-}
