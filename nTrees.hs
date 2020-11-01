
import Data.Monoid

data Tree0 a = Pt a deriving (Show, Eq, Ord)

instance Functor Tree0 where
    fmap f (Pt x) = Pt (f x)

instance Foldable Tree0 where
    foldMap f (Pt x) = f x

data Tree1 a = Leaf1 | Node1 a (Tree0 (Tree1 a)) deriving (Show, Eq, Ord)

instance Functor Tree1 where
    fmap f Leaf1 = Leaf1
    fmap f (Node1 x ys) = Node1 (f x) (fmap (fmap f) ys)

instance Foldable Tree1 where
    foldMap f Leaf1 = mempty
    foldMap f (Node1 x ys) = (f x) `mappend` (foldMap (foldMap f) ys)

data Tree2 a = Leaf2 | Node2 a (Tree1 (Tree2 a)) deriving (Show, Eq, Ord)

instance Functor Tree2 where
    fmap f Leaf2 = Leaf2
    fmap f (Node2 x ys) = Node2 (f x) (fmap (fmap f) ys)

instance Foldable Tree2 where
    foldMap f Leaf2 = mempty
    foldMap f (Node2 x ys) = (f x) `mappend` (foldMap (foldMap f) ys)

data Tree3 a = Leaf3 | Node3 a (Tree2 (Tree3 a)) deriving (Show, Eq, Ord)

instance Functor Tree3 where
    fmap f Leaf3 = Leaf3
    fmap f (Node3 x ys) = Node3 (f x) (fmap (fmap f) ys)

instance Foldable Tree3 where
    foldMap f Leaf3 = mempty
    foldMap f (Node3 x ys) = (f x) `mappend` (foldMap (foldMap f) ys)

-- functions to more easily construct low-dimensional trees
tree0 :: a -> Tree0 a
tree0 = Pt

tree1 :: [a] -> Tree1 a
tree1 [] = Leaf1
tree1 (x:xs) = Node1 x $ tree0 $ tree1 xs

-- extract the head (or root) of an n-tree
head0 :: Tree0 a -> Maybe a
head0 (Pt x) = Just x

head1 :: Tree1 a -> Maybe a
head1 Leaf1 = Nothing
head1 (Node1 x ys) = Just x

head2 :: Tree2 a -> Maybe a
head2 Leaf2 = Nothing
head2 (Node2 x ys) = Just x

head3 :: Tree3 a -> Maybe a
head3 Leaf3 = Nothing
head3 (Node3 x ys) = Just x

-- helper function for sorted functions below
geq :: (Ord a) => a -> Maybe a -> Bool
geq x Nothing = True
geq x (Just y) = x >= y

-- determine if an n-tree is sorted
sorted0 :: (Ord a) => Tree0 a -> Bool
sorted0 _ = True

sorted1 :: (Ord a) => Tree1 a -> Bool
sorted1 Leaf1 = True
sorted1 (Node1 x ys) = (sorted0 ys) && (all sorted1 ys) && (all (geq x . head1) ys)

sorted2 :: (Ord a) => Tree2 a -> Bool
sorted2 Leaf2 = True
-- is list of trees sorted, and are all the trees in the list sorted trees, and is the head geq than the head of all those subtrees?
sorted2 (Node2 x ys) = (sorted1 ys) && (all sorted2 ys) && (all (geq x . head2) ys)

sorted3 :: (Ord a) => Tree3 a -> Bool
sorted3 Leaf3 = True
sorted3 (Node3 x ys) = (sorted2 ys) && (all sorted3 ys) && (all (geq x . head3) ys)

least = Leaf2 :: Tree2 ()
next = Node2 () $ tree1 []
next2 = Node2 () $ tree1 [Leaf2]
next3 = Node2 () $ tree1 [Leaf2, Leaf2]
next4 = Node2 () $ tree1 [Leaf2, Leaf2, Leaf2]
omega = Node2 () $ tree1 [Node2 () $ tree1 []]
omega1 = Node2 () $ tree1 [Node2 () $ tree1 [], Leaf2]

down1 = Node2 () $ tree1 [next]
down2 = Node2 () $ tree1 [least, next]
down3 = Node2 () $ tree1 [least, least, next]
down4 = Node2 () $ tree1 [least, least, least, next]
down5 = Node2 () $ tree1 [least, least, least, least, next]

main = do
        putStrLn $ show $ sorted2 least
        putStrLn $ show $ sorted2 next
        putStrLn $ show $ sorted2 next2
        putStrLn $ show $ sorted2 next3
        putStrLn $ show $ sorted2 next4
        putStrLn $ show $ sorted2 omega
        putStrLn $ show $ sorted2 omega1
        putStrLn $ "downs"
        putStrLn $ show $ sorted2 down1
        putStrLn $ show $ sorted2 down2
        putStrLn $ show $ sorted2 down3
        putStrLn $ show $ sorted2 down4
        putStrLn $ show $ sorted2 down5
        --putStrLn $ show $ fmap (3+) $ tree1 [2,3,4]
        --putStrLn $ show $ fmap (3+) $ Node2 1 $ tree1 [Node2 2 Leaf1, (Node2 7 $ tree1 [Leaf2, Leaf2, Node2 9 Leaf1]), Leaf2]
        --putStrLn $ show $ getAll $ foldMap (\x -> All (x > 0)) $ Node2 1 $ tree1 [Node2 2 Leaf1, (Node2 7 $ tree1 [Leaf2, Leaf2, Node2 9 Leaf1]), Leaf2]
        --putStrLn $ show $ minimum $ Node2 2 $ tree1 [Node2 5 Leaf1, (Node2 7 $ tree1 [Leaf2, Leaf2, Node2 0 Leaf1]), Leaf2]
        --putStrLn "lists"
        --putStrLn $ show $ sorted1 $ tree1 [2,3,1,4]
        --putStrLn $ show $ sorted1 $ tree1 [1,2,3,4]
        --putStrLn "list of trees"
        --putStrLn $ show $ sorted1 $ tree1 [Leaf2, Node2 2 Leaf1, (Node2 5 $ tree1 [Leaf2, Leaf2, Node2 7 Leaf1])]
        --putStrLn $ show $ fmap sorted2 $ tree1 [Leaf2, Node2 2 Leaf1, (Node2 5 $ tree1 [Leaf2, Leaf2, Node2 7 Leaf1])]
        --putStrLn $ show $ sorted2 $ Node2 5 $ tree1 [Leaf2, Leaf2, Node2 7 Leaf1]
        --putStrLn $ show $ sorted1 $ tree1 [Leaf2, Leaf2, Node2 7 Leaf1]
        --putStrLn "trees"
        --putStrLn $ show $ sorted2 $ Node2 2 $ tree1 [Node2 5 Leaf1, (Node2 7 $ tree1 [Leaf2, Leaf2, Node2 0 Leaf1]), Leaf2]
        --putStrLn $ show $ sorted2 $ Node2 0 $ tree1 [Leaf2, Node2 2 Leaf1, (Node2 5 $ tree1 [Leaf2, Leaf2, Node2 7 Leaf1])]
