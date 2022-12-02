module AdventOfCode.Day2 where

import Text.Parsec

-- day 2
psr :: String -> Int
psr raw = 
    case parse pResults "" raw of
        Left e -> error $ show e
        Right results -> foldl (\s r -> s + score (backfill r)) 0 results

backfill :: (PSR, Result) -> (PSR, PSR)
backfill (Paper, Draw) = (Paper, Paper)
backfill (Paper, Win) = (Paper, Scissors)
backfill (Paper, Lose) = (Paper, Rock)
backfill (Rock, Draw) = (Rock, Rock)
backfill (Rock, Win) = (Rock, Paper)
backfill (Rock, Lose) = (Rock, Scissors)
backfill (Scissors, Draw) = (Scissors, Scissors)
backfill (Scissors, Win) = (Scissors, Rock)
backfill (Scissors, Lose) = (Scissors, Paper)

score :: (PSR, PSR) -> Int
score r@(x, y)= shapeScore y + roundScore r

type Parser a = Parsec a ()

shapeScore :: PSR -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

roundScore :: (PSR, PSR) -> Int
roundScore (Paper, Scissors) = 6
roundScore (Paper, Paper) = 3
roundScore (Paper, Rock) = 0
roundScore (Rock, Scissors) = 0
roundScore (Rock, Paper) = 6
roundScore (Rock, Rock) = 3
roundScore (Scissors, Scissors) = 3
roundScore (Scissors, Paper) = 0
roundScore (Scissors, Rock) = 6

data PSR = Paper | Scissors | Rock
data Result = Win | Lose | Draw

pPsr :: Parser String PSR
pPsr = choice [
    pure Paper <$> oneOf "BY",
    pure Scissors <$> oneOf "CZ",
    pure Rock <$> oneOf "AX"
    ]

pResult = choice $ (\(c, r) -> pure r <$> char c) <$> [('X', Lose), ('Y', Draw), ('Z', Win)]

pPsrPair :: Parser String (PSR, PSR)
pPsrPair = do
    psr1 <- pPsr
    _ <- space
    psr2 <- pPsr
    return (psr1, psr2)

pPsrs :: Parser String [(PSR, PSR)]
pPsrs = many $ pPsrPair <* optional endOfLine

pResults :: Parser String [(PSR, Result)]
pResults = many $ pPsrResult <* optional endOfLine

pPsrResult :: Parser String (PSR, Result)
pPsrResult = do
    psr <- pPsr
    _ <- space
    result <- pResult
    return (psr, result)
