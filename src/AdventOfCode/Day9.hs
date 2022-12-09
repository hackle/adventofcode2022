module AdventOfCode.Day9 where

import qualified Data.Map as M
import Control.Monad.Trans.State.Lazy
import Control.Monad
import Data.List (nub, reverse, last)
-- import Debug.Trace (trace)

trace = flip const
        
bridgeCross n = (runCommands n) . lines

type Coord = (Int, Int)
type Offset = Coord

-- M.filter (\(a, b) -> a /= b) $ M.mapWithKey (\k v -> (moveForDiff k,  v)) moves
moveForDiff (h, t) =
    case (abs h, abs t) of
        (1, t1) | t1 < 2 -> (0, 0)
                | otherwise -> (h, t `div` 2)
        (h1, 1) | h1 < 2 -> (0, 0)
                | otherwise -> (h `div` 2, t)
        _ -> (h `div` 2, t `div` 2)

calcDiff :: Coord -> Coord -> Offset
calcDiff (r1, c1) (r2, c2) = (r1 - r2, c1 - c2)

move :: Coord -> Offset -> Coord
move (r, c) (r1, c1) = (r + r1, c + c1)

follow :: Coord -> Coord -> Coord
follow h t = 
    let diff = calcDiff h t
        offset = moveForDiff diff
    in move t offset

stepOffset :: M.Map Char Offset
stepOffset = M.fromList [
    ('U', (-1, 0)),
    ('D', (1, 0)),
    ('L', (0, -1)),
    ('R', (0, 1))
    ]

type AppState = [[Coord]]

runOneCommand :: [Char] -> State AppState ()
runOneCommand (d:' ':n) = do
    let tot = read n ::Int
        os = stepOffset M.! d
    replicateM_ tot (chain os)
    return ()

chain :: Coord -> State AppState ()
chain os = do
    histo <- get
    let ((h:ts):_) = histo
        h1 = move h os 
        final = foldl followPrev [h1] ts
    modify $ (reverse final : ) -- head at the back, need reversing
        
followPrev :: [Coord] -> Coord -> [Coord]
followPrev prev@(h:_) cur = follow h cur : prev

runCommands :: Int -> [String] -> Int
runCommands n commands = length $ nub tailFootprint
    where
        (_, footprint) = runState (mapM_ runOneCommand commands) [take n $ repeat (0, 0)]
        tailFootprint = last <$> footprint

moveForDiff1 d = moves M.! d

moves :: M.Map Coord Coord
moves = M.fromList [
    ((-1, 0), (0, 0)),
    ((-1, -1), (0, 0)),
    ((-1, 1), (0, 0)),
    ((0, 1), (0, 0)),
    ((0, 0), (0, 0)),   --starting position
    ((0, -1), (0, 0)),
    ((1, 0), (0, 0)),
    ((1, -1), (0, 0)),
    ((1, 1), (0, 0)),
    ((-2, 0), (-1, 0)),
    ((-2, -1), (-1, -1)),
    ((-2, 1), (-1, 1)),
    ((2, 0), (1, 0)),
    ((2, -1), (1, -1)),
    ((2, 1), (1, 1)),
    ((-1, -2), (-1, -1)),
    ((0, -2), (0, -1)),
    ((1, -2), (1, -1)),
    ((-1, 2), (-1, 1)),
    ((0, 2), (0, 1)),
    ((1, 2), (1, 1)),
    ((2, 2), (1, 1)),
    ((-2, 2), (-1, 1)),
    ((-2, -2), (-1, -1)),
    ((2, -2), (1, -1))
    ]

testInput = "R 4\n\
\U 4\n\
\L 3\n\
\D 1\n\
\R 4\n\
\D 1\n\
\L 5\n\
\R 2"

testInputRound2 = "R 5\n\
\U 8\n\
\L 8\n\
\D 3\n\
\R 17\n\
\D 10\n\
\L 25\n\
\U 20"