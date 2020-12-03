import Control.Monad
import Data.List
import Combinatorics


pairs :: [Int] -> [(Int,Int)]
pairs inp = do
  x <- inp
  y <- inp
  return (x,y)

solve1 :: [Int] -> Int
solve1 inp = head $ map (\(x,y) -> x * y) $
          filter (\(x,y) -> x + y == 2020) $
          ((,) <$> inp <*> inp)
    
threeTuples :: [Int] -> [[Int]]
threeTuples inp = do
  x <- inp
  y <- inp
  z <- inp 
  return [x,y,z]


solveN ::  Int -> [Int] -> Int
solveN n =
    (product . head . filter ((== 2020) . sum)) . (tuples n)


main :: IO ()
main = do
  input <- map read <$> lines <$> getContents
  print "Solution 1:"
  print $ solveN 2 input
  print "Solution 2:"
  print $ solveN 3 input 
  
    
