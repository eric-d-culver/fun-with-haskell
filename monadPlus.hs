
import GHC.Base
import Control.Monad 

lst = [1,2,3]
lst2 = [4,5,6]

plusOrMinus :: Int -> [Int]
plusOrMinus x = [x+1, x-1]

timesTen :: Int -> [Int]
timesTen x = [x*10]


randomWalk :: Int -> Int -> [Int]
randomWalk 0 x = [x]
randomWalk n x = do
                  res <- plusOrMinus x
                  randomWalk (n-1) res

fault :: Int -> [Int]
fault _ = []

test :: Int -> [(Int, Int)]
test x = do
          res <- (plusOrMinus x) <|> (timesTen x)
          res2 <- (fault res) <|> (plusOrMinus res)
          return (res, res2)

main = do
        putStrLn $ show $ (lst <|> lst2)
        putStrLn $ show $ (randomWalk 2 0) <|> (randomWalk 3 2)
        putStrLn $ show $ test 7
