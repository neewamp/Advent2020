-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Text (pack, unpack, replace)

import Data.List.Split 
-- Might go back and use parser combinators 
-- import Text.Megaparsec hiding (count)
-- import Text.Megaparsec.Char
-- import Control.Monad
-- import Control.Applicative hiding (many)
-- import Data.Void
-- import qualified Text.Megaparsec.Char.Lexer as L

import Data.Map (Map)
import qualified Data.Map as Map

-- type Parser = Parsec Void String
-- sc :: Parser ()
-- sc = L.space space1 empty empty 
-- lexeme = L.lexeme sc

-- integer :: Parser Int
-- integer = lexeme L.decimal

-- 1-9 x: xwjgxtmrzxzmkx
-- parse :: Parser ((Int,Int), Char, String)
-- parse = do
--   low <- integer
--   _ <- char '-'
--   high <- integer
--   key <- satisfy (\_ ->True)
--   _ <- char ':'
--   rest <- many $ satisfy (\_ -> True)
--   return ((low,high), key, rest)
-- Maybe I should start using Text?  This is from stack overflow

type Graph v edges weights = Map v [(edges, weights)]

type SGraph = Graph String String Int

replaceWith :: String -> String -> String -> String
replaceWith target with  =
    unpack . (replace (pack target) (pack with)) . pack

mkEdge :: String -> SGraph
mkEdge inp =
    let [vertex, edges'] = splitOn " bags contain " (init inp) in
    case edges' == "no other bags" of
      False ->
          let splitE :: [String] = fmap (replaceWith " bag" "") $ fmap (replaceWith " bags" "") $ splitOn ", " edges' in
          let edges = fmap (\elt -> (tail $ tail elt, read $ pure $ head elt)) splitE in
          Map.insert vertex edges Map.empty
      True ->
          Map.insert vertex [] Map.empty

mkGraph :: [String] -> SGraph
mkGraph = Map.unions . fmap mkEdge 

searchForGold :: String ->  SGraph -> Bool 
searchForGold target g =
    let edgesOfTarget = fst <$> (Map.findWithDefault [] target g) in
    ("shiny gold"  `elem` edgesOfTarget) ||
    (any ((flip searchForGold) g ) edgesOfTarget)

nodesConnected :: String -> SGraph -> Int
nodesConnected target' g' = nodesConnected' target' g' - 1
    where
      nodesConnected' target g = 
          let edgesOfTarget = (Map.findWithDefault [] target g) in
          case edgesOfTarget == [] of
            True -> 1
            False ->
                1 + (sum $ fmap (\(key,v) -> v * (nodesConnected' key g)) edgesOfTarget)
          
toInt True = 1
toInt _ = 0

solve1 :: [String] -> Int
solve1 inp =
    let g = mkGraph inp in 
    let ks = Map.keys g in
    sum (fmap toInt (fmap ((flip searchForGold) g) ks))

solve2 :: [String] -> Int
solve2 inp =
    let g = mkGraph inp in 
    let ks = Map.keys g in
    nodesConnected "shiny gold" g 

main = do
  input <- lines <$> getContents
  print $ solve1 input
  print $ solve2 input
