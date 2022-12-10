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
    pairs <- parse pairs "" raw
    return $ length $ filter hasOverlap pairs

hasOverlap :: (Range, Range) -> Bool
hasOverlap (range1, range2) = overlaps range1 range2 || overlaps range2 range1 
    where
        overlaps r1@(s1, e1) r2@(s2, e2) = between s2 s1 e1 || between e2 s1 e1
        between a b c = a >= b && a <= c

round1 raw = do
    pairs <- parse pairs "" raw
    return $ length $ filter needsClean pairs

needsClean :: (Range, Range) -> Bool
needsClean (range1, range2) = 
    contains range1 range2 || contains range2 range1 
    where
        contains r1@(s1, e1) r2@(s2, e2) = (s1 >= s2 && e1 <= e2) 

int :: Parser Int
int = read @Int <$> many1 digit

around :: Parser a -> Parser b -> Parser (a, a)
around p sep = do
    r1 <- p
    sep
    r2 <- p
    return (r1, r2)

range :: Parser Range
range = int `around` (char '-')

rangePair :: Parser (Range, Range)
rangePair = range `around` (char ',')

pairs :: Parser [(Range, Range)]
pairs = rangePair `sepBy1` endOfLine

testInput= "2-4,6-8\n\
            \2-3,4-5\n\
            \5-7,7-9\n\
            \2-8,3-7\n\
            \6-6,4-6\n\
            \2-6,4-8"

    