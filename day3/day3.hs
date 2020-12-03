import Data.List

followSlope line input =
    (\(x,y) -> ((input !! y) !! x)) <$> line 
         
solve :: Int -> Int -> [[Int]] -> Int
solve x y input =
    let line = zip [0,x..] [0,y..(length input) - 1] in 
    sum $ followSlope line input 

main :: IO ()
main = do
  input <- fmap (fmap toNum) <$> lines <$> getContents
  let cycledInput = fmap cycle input
  
  putStr "Part 1: " 
  print $ solve 3 1 cycledInput
  putStr "Part 2: " 
  print $ product $ (flip (uncurry solve) cycledInput) <$> slopes

  where
    slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
    toNum :: Char -> Int
    toNum '.' = 0  
    toNum '#' = 1

