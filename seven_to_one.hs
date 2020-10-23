
{-# LANGUAGE TypeOperators #-}

import Data.Bifunctor (Bifunctor, bimap, first, second)

data a :+ b = L a | R b deriving Show

infixr 1 :+

instance Bifunctor (:+) where
        bimap f g (L x) = L (f x)
        bimap f g (R y) = R (g y)

commute :: a :+ b -> b :+ a
commute (L a) = R a
commute (R b) = L b

associate :: a :+ (b :+ c) -> (a :+ b) :+ c
associate (L a) = L (L a)
associate (R (L b)) = L (R b)
associate (R (R c)) = R c

associate' :: (a :+ b) :+ c -> a :+ (b :+ c)
associate' (L (L a)) = L a
associate' (L (R b)) = R (L b)
associate' (R c) = R (R c)

data T = Leaf | Fork T T

prettyString :: Int -> T -> String
prettyString n Leaf = replicate n '\t' ++ "*\n"
prettyString n (Fork a b) = replicate n '\t' ++ "x\n" ++ prettyString (n+1) a ++ prettyString (n+1) b 

instance Show T where
        show t = prettyString 0 t

type T0 = ()
type T1 = (T, T0)
type T2 = (T, T1)
type T3 = (T, T2)
type T4 = (T, T3)
type T5 = (T, T4)
type T6 = (T, T5)
type T7 = (T, T6)
type T8 = (T, T7)

assemble :: a :+ (T, (T, a)) -> (T, a)
assemble (L x) = (Leaf, x)
assemble (R (a, (b, x))) = (Fork a b, x)

assemble' :: (T, a) -> a :+ (T, (T, a))
assemble' (Leaf, x) = L x
assemble' (Fork a b, x) = R (a, (b, x))

liftedAssemble' = associate' . first assemble'

expand :: T7 -> T0 :+ T2 :+ T3 :+ T4 :+ T5 :+ T6 :+ T7 :+ T8
expand = liftedAssemble' . liftedAssemble' . liftedAssemble' . liftedAssemble' . liftedAssemble' . liftedAssemble' . assemble'

liftedAssemble = first (assemble . commute) . associate

contract :: T8 :+ T6 :+ T5 :+ T4 :+ T3 :+ T2 :+ T1 :+ T0 -> T1
contract = assemble . commute . liftedAssemble . liftedAssemble . liftedAssemble . liftedAssemble . liftedAssemble . liftedAssemble

test = Fork (Fork Leaf Leaf) (Fork Leaf (Fork Leaf Leaf))
test7 = (test, (test, (test, (test, (test, (test, (test, ())))))))

testShow :: (Show a) => a -> IO ()
testShow = putStrLn . show

main = do
        testShow $ assemble' (test, ())
        testShow $ expand test7
        return 0
