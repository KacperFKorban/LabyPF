data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT _ l r) = 1 + max (depthOfBT l) (depthOfBT r)

flattenBTpre :: BinTree a -> [a]
flattenBTpre EmptyBT = []
flattenBTpre (NodeBT x l r) = [x] ++ flattenBTpre l ++ flattenBTpre r

flattenBTin :: BinTree a -> [a]
flattenBTin EmptyBT = []
flattenBTin (NodeBT x l r) = flattenBTpre l ++ [x] ++ flattenBTpre r

flattenBTpost :: BinTree a -> [a]
flattenBTpost EmptyBT = []
flattenBTpost (NodeBT x l r) = flattenBTpre l ++ flattenBTpre r ++ [x]

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ EmptyBT = EmptyBT
mapBT f (NodeBT x l r) = NodeBT (f x) (mapBT f l) (mapBT f r)

isEmpty :: BinTree a -> Bool
isEmpty EmptyBT = True
isEmpty _ = False

insert :: Ord a => a -> BinTree a -> BinTree a
insert x EmptyBT = NodeBT x EmptyBT EmptyBT
insert x (NodeBT a l r) = if x > a
                            then NodeBT a l (insert x r)
                            else NodeBT a (insert x l) r

list2BST :: Ord a => [a] -> BinTree a
list2BST = go EmptyBT
    where go acc []     = acc
          go acc (x:xs) = go (insert x acc) xs

data Expr a = Lit a |
              Expr a :+: Expr a |
              Expr a :-: Expr a |
              Expr a :*: Expr a

eval :: Num a => Expr a -> a
eval (Lit n)           = n
eval (e1 :+: e2)       = eval e1 + eval e2
eval (e1 :-: e2) = eval e1 - eval e2
eval (e1 :*: e2)  = eval e1 * eval e2

show' :: Show a => Expr a -> String
show' (Lit n)                     = show n
show' (e1 :+: e2)                 = show' e1 ++ "+" ++ show' e2
show' (e1 :-: e2@(Lit _))         = show' e1 ++ "-" ++ show' e2
show' (e1 :-: e2)                 = "(" ++ show' e1 ++ ")" ++ "-" ++ "(" ++ show' e2 ++ ")"
show' (e1@(Lit _) :*: e2@(Lit _)) = show' e1 ++ "*" ++ show' e2
show' (e1@(Lit _) :*: e2)         = show' e1 ++ "*" ++ "(" ++ show' e2 ++ ")"
show' (e1 :*: e2@(Lit _))         = "(" ++ show' e1 ++ ")" ++ "*" ++ show' e2
show' (e1 :*: e2)                 = "(" ++ show' e1 ++ ")" ++ "*" ++ "(" ++ show' e2 ++ ")"

occurs :: Eq a => a -> BinTree a -> Int
occurs _ EmptyBT      = 0
occurs x (NodeBT a l r) = if x == a
                            then 1 + res
                            else res
                                where res = occurs x l + occurs x r

elemOf :: Eq a => a -> BinTree a -> Bool
elemOf _ EmptyBT = False
elemOf x (NodeBT a l r) = if x == a
                            then True
                            else elemOf x l || elemOf x r

reflect :: BinTree a -> BinTree a
reflect EmptyBT = EmptyBT
reflect (NodeBT x l r) = NodeBT x (reflect r) (reflect l)

minElemOf :: Ord a => BinTree a -> a
minElemOf EmptyBT = error "minElemOf: empty tree has no minimal element"
minElemOf (NodeBT x EmptyBT EmptyBT) = x
minElemOf (NodeBT x EmptyBT r) = min x (minElemOf r)
minElemOf (NodeBT x l EmptyBT) = min x (minElemOf l)
minElemOf (NodeBT x l r) = minimum [x, minElemOf l, minElemOf r]

maxElemOf :: Ord a => BinTree a -> a
maxElemOf EmptyBT = error "minElemOf: empty tree has no minimal element"
maxElemOf (NodeBT x EmptyBT EmptyBT) = x
maxElemOf (NodeBT x EmptyBT r) = max x (minElemOf r)
maxElemOf (NodeBT x l EmptyBT) = max x (minElemOf l)
maxElemOf (NodeBT x l r) = maximum [x, minElemOf l, minElemOf r]

foldBinTree :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldBinTree _ z EmptyBT = z
foldBinTree f z (NodeBT x l r) = f x (foldBinTree f z l) (foldBinTree f z r)

mapBT' :: (a -> b) -> BinTree a -> BinTree b
mapBT' f = foldBinTree (\x y z -> NodeBT (f x) y z) EmptyBT

data GTree a = Leaf a |
               GNode [GTree a]
               deriving Show

sumGTree :: Num a => GTree a -> a
sumGTree (Leaf a)   = a
sumGTree (GNode xs) = sum $ map sumGTree xs

elemOfGTree :: Eq a => a -> GTree a -> Bool
elemOfGTree x (Leaf a)   = x == a
elemOfGTree x (GNode xs) = any (elemOfGTree x) xs

depthOfGTree :: GTree a -> Int
depthOfGTree (Leaf _)      = 1
depthOfGTree (GNode xs) = 1 + (maximum $ map depthOfGTree xs)

mapGTree :: (a -> b) -> GTree a -> GTree b
mapGTree f (Leaf x)   = Leaf $ f x
mapGTree f (GNode xs) = GNode $ map (mapGTree f) xs

flattenGTree :: GTree a -> [a]
flattenGTree (Leaf a)   = [a]
flattenGTree (GNode xs) = concat $ map flattenGTree xs

countGTreeLeaves :: GTree a -> Int
countGTreeLeaves (Leaf a) = 1
countGTreeLeaves (GNode xs) = sum $ map countGTreeLeaves xs

instance Eq a => Eq (BinTree a) where
    EmptyBT == EmptyBT = True
    EmptyBT == _       = False
    _ == EmptyBT       = False
    (NodeBT x1 l1 r1) == (NodeBT x2 l2 r2) = x1 == x2 && l1 == l2 && r1 == r2
