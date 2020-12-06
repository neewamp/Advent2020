import Data.List
decode_row :: Int -> Int -> String -> Int
decode_row low _ "F" = low
decode_row _ upper "B" = upper
decode_row low upper (x:xs) =
    let mid = (div (upper-low) 2) + low in 
    case x of
      'F' -> decode_row low mid xs
      'B' -> decode_row (mid+1) upper xs
      c -> 1000000000000

decode_col :: Int -> Int -> String -> Int
decode_col low _ "L" = low
decode_col _ upper "R" = upper
decode_col low upper (x:xs) =
    let mid = (div (upper-low) 2) + low in 
    case x of
      'L' -> decode_col low mid xs
      'R' -> decode_col (mid+1) upper xs
      c -> 1000000000000

ex = "FBFBBFFRLR"
decode :: String -> Int
decode pass =
    let (row,col) = splitAt ((length pass) - 3) pass in 
    let rowN = decode_row 0 127 row in
    let colN = decode_col 0 7 col in
    rowN * 8 + colN
    
solve :: [String] -> Int 
solve = maximum . fmap decode 

solve2 :: [Int] -> Int
solve2 seats =
    let min = minimum seats in
    let seats' = fmap (\elt -> elt - min) seats in 
    min + (head $
         filter (\elt -> not $ elem elt seats') [1..((length seats) -2)])

main = do
  passes <- lines <$> getContents
  print $ solve passes
  let seats = fmap decode passes
  print $ sort seats
  print $ solve2 (fmap decode passes)
