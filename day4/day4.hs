{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad

import Data.List.Split
import Data.Foldable
import Data.List
import Data.Monoid
import Data.Char

import Text.Read

fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isJustTrue :: Maybe Bool -> Bool
isJustTrue (Just True) = True
isJustTrue _ = False

correctNum :: Int -> Int -> String -> Maybe Bool
correctNum low high rest =
    fmap (\i -> (i >= low && i <= high))
         ((readMaybe rest) :: Maybe Int)

validHeight :: String -> Bool
validHeight h
    | "ni" `isPrefixOf` revH =
           isJustTrue $ join $ 
            fmap (correctNum 59 76)
            (reverse <$> (stripPrefix "ni" revH))
    | "mc" `isPrefixOf` revH =
        isJustTrue $ join $ 
        fmap (correctNum 150 193)
         (reverse <$> (stripPrefix "mc" revH))
    | otherwise = False
    where
      revH = reverse h

validHair :: String -> Bool
validHair hair =
    (length hair == 6) &&
    all (\elt ->
     elt `elem` ['0' .. '9'] || ('a' <= elt && elt <= 'f')) hair 


validEye :: String -> Bool
validEye color =
    any (== color) eyeColors
    where
      eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPid :: String -> Bool
validPid pid = 
    all isDigit pid && length pid == 9
    
valid2 :: String -> Bool
valid2 pass
    | "byr:" `isPrefixOf` pass = 
        -- Gross
        isJustTrue $ join $
         fmap (correctNum 1920 2002) (stripPrefix "byr:" pass)
    | "iyr:" `isPrefixOf` pass =
        isJustTrue $ join $
         fmap (correctNum 2010 2020) (stripPrefix "iyr:" pass)
    | "eyr:" `isPrefixOf` pass =
        isJustTrue $ join $
         fmap (correctNum 2020 2030) (stripPrefix "eyr:" pass)
    | "hgt:" `isPrefixOf` pass = 
        isJustTrue $ validHeight <$> (stripPrefix "hgt:" pass)
    | "hcl:#" `isPrefixOf` pass =
        isJustTrue $ validHair <$> (stripPrefix "hcl:#" pass)
    | "ecl:" `isPrefixOf` pass =
        isJustTrue $ validEye <$> (stripPrefix "ecl:" pass)
    | "pid:" `isPrefixOf` pass =
        isJustTrue $ validPid <$> (stripPrefix "pid:" pass)
    | "cid:" `isPrefixOf` pass = True
    | otherwise = False

valid :: String -> Bool
valid pass =
    getAll $ fold $ fmap
               (\elt -> All $ isInfixOf elt pass) fields

toInt True = 1
toInt False = 0

solve1 :: [String] -> Int
solve1 passwords = sum $ toInt <$> (fmap valid passwords)
    
solve2 :: [String] -> Int 
solve2 passports =
    sum $ toInt <$> fmap (\pass -> valid pass &&
                   (all valid2 $ words pass)) passports

-- my_group :: String -> [String]
-- my_group input = fmap fold $ splitOn [""] input

my_group :: String -> [String]
my_group input = splitOn "HACK" input


main = do
  l <- lines <$> getContents
  let modL = unlines $ fmap (\line -> if line == "" then "HACK" else line) l
  let modLSpaces = fmap (\c -> if c == '\n' then ' ' else c) modL
  let input = my_group $ modLSpaces
  print $ solve1 input 
  print $ solve2 input 
        
