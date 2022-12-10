module AdventOfCode.Day10 where

import Text.Parsec
import Debug.Trace
import Data.List

type Parser = Parsec String ()

data Instruction = Noop | AddX Int deriving (Eq)

instance Show Instruction where
    show Noop = "noop"
    show (AddX x) = "addx " ++ show x

-- 20 then each 40
cycles mx = takeWhile (< mx) [20, 60..]

runApp :: String -> String
runApp raw = 
    case parse pInstructions "" raw of
        Left err -> show err
        Right instructions -> 
            let finalState = runInstructions instructions
                totalStrength = calcSignalStrength finalState
                sprites = zipWith lit [0..] $ tail finalState   -- final state has a padding of the 0 step
            in  (show totalStrength) ++ "\n" ++ (intercalate "\n" $ breakAll 40 sprites)    -- this needs putStrLn to show line breaks

breakAll n xs = unfoldr goOn xs
    where 
        goOn [] = Nothing
        goOn xs = Just $ splitAt n xs 

lit currentPos spriteCentre = 
    let spritePos = (+ spriteCentre) <$> [-1, 0, 1]
        overlaps = (currentPos `mod` 40) `elem` spritePos
    in if overlaps then '#' else '.'

calcSignalStrength :: [Int] -> Int
calcSignalStrength finalState = foldl (+) 0 cycleStrength
    where
        len = length finalState - 1
        strength c = c * (finalState !! c)
        cycleStrength = strength <$> cycles len

runInstructions :: [Instruction] -> [Int]
runInstructions ix = 
    let asList = fst $ foldl runOne ([1], id) ix -- id is place holder for "setX"
    in reverse asList
    where 
        runOne (xs@(x:_), f) Noop = (f x : xs, id)
        runOne (xs@(x:_), f) (AddX n) = let cur = f x in (cur : cur : xs, (+ n))    -- setX is delayed

pNoop = string "noop" *> pure Noop
pAddx = do
    string "addx "
    sign <- many $ char '-'
    ns <- many1 digit
    let x = read ns :: Int
        base = if sign == ['-'] then -1 else 1
    pure (AddX (x * base))

pInstructions :: Parser [Instruction]
pInstructions = (pNoop <|> pAddx) `sepBy1` endOfLine <* eof

testInstructions = [
    Noop,
    AddX 3,
    AddX (-5)
    ]

testInput = "noop\n\
            \addx 3\n\
            \addx -5"

testInput2 = 
    "addx 15\n\
    \addx -11\n\
    \addx 6\n\
    \addx -3\n\
    \addx 5\n\
    \addx -1\n\
    \addx -8\n\
    \addx 13\n\
    \addx 4\n\
    \noop\n\
    \addx -1\n\
    \addx 5\n\
    \addx -1\n\
    \addx 5\n\
    \addx -1\n\
    \addx 5\n\
    \addx -1\n\
    \addx 5\n\
    \addx -1\n\
    \addx -35\n\
    \addx 1\n\
    \addx 24\n\
    \addx -19\n\
    \addx 1\n\
    \addx 16\n\
    \addx -11\n\
    \noop\n\
    \noop\n\
    \addx 21\n\
    \addx -15\n\
    \noop\n\
    \noop\n\
    \addx -3\n\
    \addx 9\n\
    \addx 1\n\
    \addx -3\n\
    \addx 8\n\
    \addx 1\n\
    \addx 5\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx -36\n\
    \noop\n\
    \addx 1\n\
    \addx 7\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx 2\n\
    \addx 6\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx 1\n\
    \noop\n\
    \noop\n\
    \addx 7\n\
    \addx 1\n\
    \noop\n\
    \addx -13\n\
    \addx 13\n\
    \addx 7\n\
    \noop\n\
    \addx 1\n\
    \addx -33\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx 2\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx 8\n\
    \noop\n\
    \addx -1\n\
    \addx 2\n\
    \addx 1\n\
    \noop\n\
    \addx 17\n\
    \addx -9\n\
    \addx 1\n\
    \addx 1\n\
    \addx -3\n\
    \addx 11\n\
    \noop\n\
    \noop\n\
    \addx 1\n\
    \noop\n\
    \addx 1\n\
    \noop\n\
    \noop\n\
    \addx -13\n\
    \addx -19\n\
    \addx 1\n\
    \addx 3\n\
    \addx 26\n\
    \addx -30\n\
    \addx 12\n\
    \addx -1\n\
    \addx 3\n\
    \addx 1\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx -9\n\
    \addx 18\n\
    \addx 1\n\
    \addx 2\n\
    \noop\n\
    \noop\n\
    \addx 9\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx -1\n\
    \addx 2\n\
    \addx -37\n\
    \addx 1\n\
    \addx 3\n\
    \noop\n\
    \addx 15\n\
    \addx -21\n\
    \addx 22\n\
    \addx -6\n\
    \addx 1\n\
    \noop\n\
    \addx 2\n\
    \addx 1\n\
    \noop\n\
    \addx -10\n\
    \noop\n\
    \noop\n\
    \addx 20\n\
    \addx 1\n\
    \addx 2\n\
    \addx 2\n\
    \addx -6\n\
    \addx -11\n\
    \noop\n\
    \noop\n\
    \noop"