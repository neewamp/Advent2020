import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set

import Data.List

-- solve1 input = 
--   let flat_input = fmap  (filter (/= '\n')) input in 
--   let input_set = fmap Set.size (fmap Set.fromList flat_input) in
--   sum input_set


questions = enumFromTo 'a' 'z'

-- Should make this a maybe and use foldM
everyone :: [String] -> Int
everyone dec =
    length $ foldl intersect (head dec) (tail dec)


anyone :: [String] -> Int
anyone dec =
    length $ foldl union (head dec) (tail dec)

solve choice input =
    let l = fmap lines input in
    sum $ fmap choice l

main = do
  input <- splitOn "\n\n" <$> getContents
  print $ solve anyone input
  print $ solve everyone input
    
