{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.List.Split
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
import Control.Monad
import Control.Applicative hiding (many)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
sc :: Parser ()
sc = L.space space1 empty empty 
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

-- 1-9 x: xwjgxtmrzxzmkx
parse :: Parser ((Int,Int), Char, String)
parse = do
  low <- integer
  _ <- char '-'
  high <- integer
  key <- satisfy (\_ ->True)
  _ <- char ':'
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

solve :: ((Int,Int) -> Char -> String -> Bool) ->
          [String] -> Int
solve valid =
    length . filter (== True) . (fmap (isValid valid)) 

main :: IO () 
main = do
  input <- lines <$> getContents
  putStrLn $ "Part1: " ++ show (solve valid1 input)
  putStrLn $ "Part2: " ++ show (solve valid2 input)

-- main :: IO ()
-- main = do
--   input <- fmap words <$> lines <$> getContents
--   print $ solve2 input 
