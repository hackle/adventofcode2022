module AdventOfCode.Day23 where

import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad (mplus)
import Debug.Trace
import Data.Foldable (Foldable(minimum, maximum))

type Coord = (Int, Int)
type Directions = [[Coord]]
type Grid = M.Map Coord Int

solve grid n = total - M.size grid
    where 
        (_, final) = L.iterate next (dirs, grid) !! n
        [rs, cs] = [ fn <$> M.keys final | fn <- [fst, snd] ]
        [r1, r2] = traceShowId [minimum rs, maximum rs]
        [c1, c2] = traceShowId [minimum cs, maximum cs]
        total = (r2 - r1 + 1) * (c2 - c1 + 1)

solve2 grid = let (x:xs) = snd <$> L.iterate next (dirs, grid) in go 1 x xs
    where 
        go n y (x:xs) = if y == x then n else go (n + 1) x xs

next :: (Directions, Grid) -> (Directions, Grid)
next (ds, grid) = (rotateDirs ds, M.fromList $ M.elems $ M.mapWithKey move1 grid)
    where 
        proposed :: M.Map Coord (Coord, Int)
        proposed = M.mapWithKey try1 grid
        reverseLookup :: M.Map Coord Bool
        reverseLookup = M.map (== 1) $ M.fromListWith (+) $ M.elems proposed 
        try1 :: Coord -> Int -> (Coord, Int)
        try1 coord _ = 
            let allMoves = [tryMove coord d grid | d <- ds]
            in case (sequence allMoves, L.foldl1 mplus allMoves) of
                (Just _, _) -> (coord, 1)   -- empty on all sides, don't move
                (_, Nothing) -> (coord, 1)  -- no move possible
                (_, Just coord1) -> (coord1, 1)
        move1 :: Coord -> Int -> (Coord, Int)
        move1 coord _ = 
            let (coord1, _) = proposed M.! coord in 
                if reverseLookup M.! coord1 -- can move
                    then (coord1, 1)
                    else (coord, 1)

filters = [
    (== (-1)) . fst    -- north
    , (== 1) . fst -- south
    , (== (-1)) . snd    -- west
    , (== 1) . snd  -- east
    ]

dirs :: Directions
dirs = [ filter d offsets | d <- filters ]

tryMove :: Coord -> [(Int, Int)] -> Grid -> Maybe Coord
tryMove coord dir mx = 
    let coords = addCoords coord <$> dir
    in if any (`M.member` mx) coords
        then Nothing
        else Just $ coords L.!! 1

addCoords (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

rotateDirs (x: xs) = xs++[x]
offsets = [(r1, c1) | r1 <- [-1, 0, 1], c1 <- [-1, 0, 1], (r1, c1) /= (0, 0)]
neighbours (r, c) = [(r + r1, c + c1) | (r1, c1) <- offsets]


toGrid :: String -> Grid
toGrid raw = M.fromList [ ((r, c), 1) | (r, l) <- indexed $ lines raw, (c, ch) <- indexed l, ch /= '.' ]

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

testInput = 
    ".....\n\
    \..##.\n\
    \..#..\n\
    \.....\n\
    \..##.\n\
    \....."

testInput2 =
    "..............\n\
    \..............\n\
    \.......#......\n\
    \.....###.#....\n\
    \...#...#.#....\n\
    \....#...##....\n\
    \...#.###......\n\
    \...##.#.##....\n\
    \....#..#......\n\
    \..............\n\
    \..............\n\
    \.............."