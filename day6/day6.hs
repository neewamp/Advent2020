import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set

import Data.List

solve1 input = 
  let flat_input = fmap  (filter (/= '\n')) input in 
  let input_set = fmap Set.size (fmap Set.fromList flat_input) in
  sum input_set


questions = enumFromTo 'a' 'z'

everyone :: [String] -> Int
everyone dec =
    foldl (\acc letter ->
               if all (\elt -> letter `elem` elt) dec
               then 1 + acc else acc)
    0 questions

solve2 input =
    let l = fmap lines input in
    sum $ fmap everyone l 
    

main = do
  input <- splitOn "\n\n" <$> getContents
  print $ solve1 input
  print $ solve2 input
    
