-- allows Functor typeclass to be derived automatically
{-# LANGUAGE DeriveFunctor #-}

import Testing(testInverses)

-- practice with recursive data types
-- List: x = 1 + a*x
data List a = Nil | Cons a (List a)

head' :: List a -> Maybe a
head' Nil = Nothing
head' (Cons x ys) = Just x

tail' :: List a -> List a
tail' Nil = Nil
tail' (Cons x ys) = ys

toList :: List a -> [a]
toList Nil = []
toList (Cons x ys) = x:(toList ys)

frList :: [a] -> List a
frList [] = Nil
frList (x:ys) = Cons x (frList ys)

-- Tree: x = a + a*x*x
data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show, Functor)

-- Functor inside a Functor: TreeWriter a
-- data Writer a = Writer (a, String) deriving (Show, Functor)
-- type TreeWriter a = Compose Tree Writer a

-- type version of isomorphism x*(y+z) = (x*y)+(x*z)
prodToSum :: (a, Either b c) -> Either (a, b) (a, c)
prodToSum (x, e) =
    case e of 
        Left y -> Left (x, y)
        Right z -> Right (x, z)

sumToProd :: Either (a, b) (a, c) -> (a, Either b c)
sumToProd e =
    case e of 
        Left (x, y) -> (x, Left y)
        Right (x, z) -> (x, Right z)

-- Maybe a isomorphic to Either () a 
-- type version of isomorphism 0+x = x
maybeToSum :: Maybe a -> Either () a
maybeToSum Nothing = Left ()
maybeToSum (Just x) = Right x

sumToMaybe :: Either () a -> Maybe a
sumToMaybe (Left ()) = Nothing
sumToMaybe (Right x) = Just x

-- type version of isomorphism x+x = 2*x (where Bool is 2)
sumToDub :: Either a a -> (Bool, a)
sumToDub (Left x) = (False, x)
sumToDub (Right x) = (True, x)

dubToSum :: (Bool, a) -> Either a a
dubToSum (False, x) = Left x
dubToSum (True, x) = Right x

lst = frList [1,1,2,3,5,8]

tree = Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 7)

-- treeWrite = Node (Writer (1, "Hello")) (Node (Writer (2, "World")) (Leaf (Writer (3, "Goodbye"))) (Leaf (Writer (4, "Home")))) (Leaf (Writer (7, "Salad"))) :: TreeWriter Int

tst = (1, Left 2.1) :: (Int, Either Double String)
tet = Right (2, "Hello") :: Either (Int, Double) (Int, String)

thing = Just 5
thing2 = Nothing :: Maybe Int

stuff = Left 1 :: Either Int Int
stuff2 = (True, 17)

testVal = 3

main = do
        case testVal of
            0 -> putStrLn "Hello, World!"
            1 -> do
                putStrLn $ show $ head' lst
                putStrLn $ show $ toList $ tail' lst
            2 -> do
                putStrLn $ show $ fmap (3-) tree
            3 -> do
                testInverses prodToSum sumToProd tet
                testInverses sumToProd prodToSum tst
            4 -> do
                testInverses sumToMaybe maybeToSum thing
                testInverses sumToMaybe maybeToSum thing2
            5 -> do
                testInverses dubToSum sumToDub stuff
                testInverses sumToDub dubToSum stuff2
