module AdventOfCode.Day3 where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char

rucksack :: String -> Int
rucksack raw = round2 $ lines raw

round2 sacks = sum $ (toPriority . findBadge) <$> sackGroups where
    sackGroups = L.unfoldr (tryTake 3) sacks
    tryTake n (x:y:z:rest) = Just (x:y:z:[], rest)
    tryTake _ _ = Nothing

round1 sacks = sum $ (toPriority . findDupInSack) <$> sacks

toPriority :: Char -> Int
toPriority c = ord c - baseA where
    baseA = if c >= 'a' && c <= 'z' 
            then ord 'a' - 1 
            else ord 'A' - 27

findBadge :: [String] -> Char
findBadge (xs:ys:rest) = head $ L.foldl findDups (findDups xs ys) rest
findBadge _ = error "Not enough sacks"

findDupInSack :: String -> Char
findDupInSack str = head $ findDups firstHalf secondHalf where
    (firstHalf, secondHalf) = L.splitAt (length str `div` 2) str

findDups :: String -> String -> [Char]
findDups xs ys = L.filter (`elem` xs) ys