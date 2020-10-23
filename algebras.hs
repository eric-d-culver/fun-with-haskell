{-# LANGUAGE DeriveFunctor #-}

import Data.List(unfoldr)

newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

data NatF a = ZeroF | SuccF a deriving Functor

fib :: NatF (Int, Int) -> (Int, Int)
fib ZeroF = (0, 1)
fib (SuccF (m, n)) = (n, m + n)

data ListF e a = NilF | ConsF e a deriving Functor

lenAlg :: ListF e Int -> Int 
lenAlg (ConsF e n) = n + 1
lenAlg NilF = 0

sumAlg :: ListF Double Double -> Double
sumAlg (ConsF e a) = e + a
sumAlg NilF = 0

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

data StreamF e a = StreamF e a deriving Functor

era :: [Int] -> StreamF Int [Int]
era (p : ns) = StreamF p (filter (notdiv p) ns)
        where notdiv p n = n `mod` p /= 0

primes = ana era [2..]

toListC :: Fix (StreamF e) -> [e]
toListC = cata al
        where al :: StreamF e [e] -> [e] 
              al (StreamF e a) = e : a

--Exercises

polyEvalAlg :: Num a => a -> ListF a a -> a
polyEvalAlg x (ConsF e a) = e + x * a
polyEvalAlg x NilF = 0

sqr :: Int -> StreamF Int Int
sqr n = StreamF (n*n) (n+1)

squares = ana sqr 1

eraM :: (Int, [Int]) -> Maybe (Int, (Int, [Int]))
eraM (0, _) = Nothing
eraM (n, (p : ns)) = Just (p, (n-1, (filter (notdiv p) ns)))
        where notdiv p n = n `mod` p /= 0

primes' n = unfoldr eraM (n, [2..])

--Testing code

fibTest = Fix $ SuccF $ Fix $ SuccF $ Fix $ SuccF $ Fix $ ZeroF
listTest = Fix $ ConsF 4 $ Fix $ ConsF 3 $ Fix $ ConsF 2 $ Fix $ ConsF 1 $ Fix $ NilF

main = do
        putStrLn $ show $ cata fib fibTest
        putStrLn $ show $ cata lenAlg listTest
        putStrLn $ show $ cata sumAlg listTest
        putStrLn $ show $ take 20 $ toListC primes
        putStrLn $ show $ cata (polyEvalAlg 1.5) listTest
        putStrLn $ show $ take 20 $ toListC squares
        putStrLn $ show $ primes' 20
