{-# language TypeApplications #-}

module AdventOfCode.Day4 where

import Text.Parsec
import Data.Function ((&))
import Data.Either.Combinators (mapLeft)

type Range = (Int, Int)
type Parser a = Parsec String () a

campCleanup :: String -> Either String Int
campCleanup = mapLeft show . round2

round2 raw = do
    pairs <- parse parsePairs "" raw
    return $ length $ filter hasOverlap pairs

hasOverlap :: (Range, Range) -> Bool
hasOverlap (range1, range2) = overlaps range1 range2 || overlaps range2 range1 
    where
        overlaps r1@(s1, e1) r2@(s2, e2) = between s2 s1 e1 || between e2 s1 e1
        between a b c = a >= b && a <= c

round1 raw = do
    pairs <- parse parsePairs "" raw
    return $ length $ filter needsClean pairs

needsClean :: (Range, Range) -> Bool
needsClean (range1, range2) = 
    contains range1 range2 || contains range2 range1 
    where
        contains r1@(s1, e1) r2@(s2, e2) = (s1 >= s2 && e1 <= e2) 

parseNum :: Parser Int
parseNum = read @Int <$> many1 digit

delimit p sep = do 
    r1 <- p
    sep
    r2 <- p
    return (r1, r2)

parseRange :: Parser Range
parseRange = parseNum `delimit` (char '-')

parseRangePair :: Parser (Range, Range)
parseRangePair = parseRange `delimit` (char ',')

parsePairs :: Parser [(Range, Range)]
parsePairs = parseRangePair `sepBy1` endOfLine

    