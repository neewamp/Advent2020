{-# LANGUAGE OverloadedStrings #-}
import Data.List.Split
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
import Control.Monad
import Control.Applicative hiding (many)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L

sc :: Parser ()
sc = L.space space1 empty empty 

lexeme = L.lexeme sc

type Parser = Parsec Void String

-- 1-9 x: xwjgxtmrzxzmkx
integer :: Parser Int
integer = lexeme L.decimal

parse :: Parser ((Int,Int), Char, String)
parse = do
  low <- integer
  _ <- char '-'
  high <- integer
  key <- satisfy (\_ ->True)
  _ <- char ':'
  sc
  rest <- many $ satisfy (\_ -> True)
  return ((low,high), key, rest)

count :: Char -> String -> Int
count char = length . filter (char ==)

valid1 :: (Int,Int) -> Char -> String -> Bool
valid1 (low, high) key pass =
    count' >= low && count' <= high
    where
      count' = count key pass
    
valid2 :: (Int,Int) -> Char -> String -> Bool
valid2 (low, high) key pass = 
    cond1 /= cond2
    where
      cond1 = (head $ drop (low-1) pass) == key
      cond2 = (head $ drop (high-1) pass) == key


isValid :: ((Int,Int) -> Char -> String -> Bool) -> 
    String -> Bool
isValid valid inp = -- (condition:key:pass:[]) =
    case runParser Main.parse "" inp of
      Left _ -> undefined
      Right ((low, high), key, pass) -> 
          valid (low, high) key pass 
    -- valid (head c, last c) k pass
    --     where
    --       c = map read $ splitOn "-" condition
    --       k = head key
isValid _ l = undefined           

solve1 :: [[String]] -> Int
solve1 = length . filter (== True) . (fmap (isValid valid1))

solve2 :: [[String]] -> Int
solve2 = length . filter (== True) . (fmap (isValid valid2))

main :: IO ()
main = do
  input <- fmap words <$> lines <$> getContents
  print $ solve2 input 
