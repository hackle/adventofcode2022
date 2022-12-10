module AdventOfCode.Day10 where

import qualified Data.Vector as V
import Text.Parsec
import Debug.Trace
import Data.List (intercalate)

type Parser = Parsec String ()

data Instruction = Noop | AddX Int deriving (Eq)

instance Show Instruction where
    show Noop = "noop"
    show (AddX x) = "addx " ++ show x

-- 20 then each 40
cycles mx = V.fromList $ takeWhile (< mx) [20, 60..]

runApp :: String -> String
runApp raw = 
    case parse pInstructions "" raw of
        Left err -> show err
        Right instructions -> 
            let finalState = runInstructions instructions
                show1 r = show r ++ (show $ strength1 r)
            in  show $ 
                calcSignalStrength $ 
                trace (intercalate "\n" $ V.toList $ show1 <$> V.indexed finalState) finalState

strength1 (idx, (x, _)) = idx * x

calcSignalStrength :: V.Vector (Int, String) -> Int
calcSignalStrength finalState = V.foldl (+) 0 cycleStrength
    where
        len = V.length finalState - 1
        strength c = c * (fst $ finalState V.! c)
        cycleStrength = strength <$> cycles len

runInstructions :: [Instruction] -> V.Vector (Int, String)
runInstructions ix = 
    let asList = foldl runOne [(1, "")] ix 
    in V.fromList $ reverse asList -- tail to remove the starting [1]
    where 
        runOne res@(h@(n, _):_) i@Noop = (n, show i):res
        runOne res@(h@(n, _):_) i@(AddX x) = (n + x, show i) : (n, show i) : res 

pNoop = string "noop" *> pure Noop
pAddx = do
    string "addx "
    sign <- many $ char '-'
    ns <- many1 digit
    let x = read ns :: Int
        base = if sign == ['-'] then -1 else 1
    pure (AddX (x * base))

pInstructions :: Parser [Instruction]
pInstructions = (pNoop <|> pAddx) `sepBy1` endOfLine

testInstructions = [
    Noop,
    AddX 3,
    AddX (-5)
    ]

testInput = "noop\n\
            \addx 3\n\
            \addx -5"

testInput2 = 
    "noop\n\
    \noop\n\
    \addx 5\n\
    \addx 21\n\
    \addx -16\n\
    \noop\n\
    \addx 1\n\
    \noop\n\
    \noop\n\
    \addx 4\n\
    \addx 1\n\
    \addx 4\n\
    \addx 1\n\
    \noop\n\
    \addx 4\n\
    \addx -9\n\
    \noop\n\
    \addx 19\n\
    \addx -5\n\
    \noop\n\
    \noop\n\
    \addx 5\n\
    \addx 1\n\
    \addx -38\n\
    \addx 5\n\
    \addx -2\n\
    \addx 2\n\
    \noop\n\
    \noop\n\
    \addx 7\n\
    \addx 9\n\
    \addx 20\n\
    \addx -3\n\
    \addx -18\n\
    \addx 2\n\
    \addx 5\n\
    \noop\n\
    \noop\n\
    \addx -2\n\
    \noop\n\
    \noop\n\
    \addx 7\n\
    \addx 3\n\
    \addx -2\n\
    \addx 2\n\
    \addx -28\n\
    \addx -7\n\
    \addx 5\n\
    \noop\n\
    \addx 2\n\
    \addx 32\n\
    \addx -27\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx 7\n\
    \noop\n\
    \addx 22\n\
    \addx -19\n\
    \noop\n\
    \addx 5\n\
    \noop\n\
    \addx -7\n\
    \addx 17\n\
    \addx -7\n\
    \noop\n\
    \addx -20\n\
    \addx 27\n\
    \noop\n\
    \addx -16\n\
    \addx -20\n\
    \addx 1\n\
    \noop\n\
    \addx 3\n\
    \addx 15\n\
    \addx -8\n\
    \addx -2\n\
    \addx -6\n\
    \addx 14\n\
    \addx 4\n\
    \noop\n\
    \noop\n\
    \addx -17\n\
    \addx 22\n\
    \noop\n\
    \addx 5\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx 2\n\
    \noop\n\
    \addx 3\n\
    \addx -32\n\
    \addx -5\n\
    \noop\n\
    \addx 4\n\
    \addx 3\n\
    \addx -2\n\
    \addx 34\n\
    \addx -27\n\
    \addx 5\n\
    \addx 16\n\
    \addx -18\n\
    \addx 7\n\
    \noop\n\
    \addx -2\n\
    \addx -1\n\
    \addx 8\n\
    \addx 14\n\
    \addx -9\n\
    \noop\n\
    \addx -15\n\
    \addx 16\n\
    \addx 2\n\
    \addx -35\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx 3\n\
    \addx 4\n\
    \noop\n\
    \addx 1\n\
    \addx 4\n\
    \addx 1\n\
    \noop\n\
    \addx 4\n\
    \addx 2\n\
    \addx 3\n\
    \addx -5\n\
    \addx 19\n\
    \addx -9\n\
    \addx 2\n\
    \addx 4\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx 3\n\
    \addx 2\n\
    \noop\n\
    \noop\n\
    \noop"