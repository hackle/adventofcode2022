module AdventOfCode.Day14 where

import qualified Data.Map as M
import Text.Parsec
import Control.Monad
import Debug.Trace

type Coord = (Int, Int) -- col, row. Col comes first!
type Bricks = M.Map Coord Char

safeRange x y = if x < y then [x..y] else [y..x]

expand :: Coord -> Coord -> [Coord]
expand (c1, r1) (c2, r2) =  -- either row or col must be equal
    if c1 == c2
        then zip (repeat c1) (safeRange r1 r2)
        else zip (safeRange c1 c2) (repeat r1)

expandPath :: [Coord] -> [Coord]
expandPath xs = concat $ zipWith expand xs (tail xs)

toBricks :: [[Coord]] -> Bricks
toBricks xs = M.fromList $ withBrick <$> concat (expandPath <$> xs)
    where withBrick x = (x, '#')

starting = (500, -1)
ending = (500, 0)
fallOrder = [(0, 1), (-1, 1), (1, 1)]

addCoord (c1, r1) (c2, r2) = (c1 + c2, r1 + r2)

lowest :: Bricks -> Int
lowest bricks = foldl1 max $ (snd . fst) <$> M.toList bricks  -- col increase downwards

fall1 :: Bricks
        -> (Int -> Bool)    -- stop per row index
        -> (Coord -> Maybe Coord)    -- rest per row index
        -> Maybe Coord   -- success or failure both in coords
fall1 bricks stop rest = go starting
    where 
        go pos@(_, r) =
            if stop r then Nothing else 
                (rest pos) `mplus`
                    (maybe
                        (Just pos)          -- when all positions are taken, rest
                        go  -- still falling, check rest position before going
                        (msum $ moves pos))
        moves pos = move1 pos <$> fallOrder
        move1 pos offset = 
            let pos1 = addCoord offset pos 
            in maybe (Just pos1) (const Nothing) (pos1 `M.lookup` bricks) -- if pos is taken, try next, if all fail
    
fall :: Bricks
        -> (Int -> Bool)    -- stop per row index
        -> (Coord -> Maybe Coord)    -- rest per row index
        -> (Int, Bricks)    -- count of sands till it fails
fall bricks stopFall rest = go 0 bricks
    where
        go cnt bricks = 
            case M.lookup ending bricks of
                Just _ -> (cnt, bricks) -- filled up
                Nothing ->    
                    case fall1 bricks stopFall rest of
                        Nothing -> (cnt, bricks)
                        Just pos -> go (cnt + 1) (M.insert pos 'o' bricks)


stopAtHighest = (== 0)
restOnBottom bottom pos@(_, r) = if r == bottom - 1 then Just pos else Nothing


runApp raw =
    case parse pScan "" raw of
        Left err -> show err
        Right paths ->
            let bricks = toBricks paths
                lowestRow = lowest bricks
                stopAtLowest = (> lowestRow)
                fallen1 = fall bricks stopAtLowest (const Nothing)
                fallen2 = fall bricks (const False) (restOnBottom $ lowestRow + 2)
            in show (fst fallen2)
    where

-- parsers

type Parser = Parsec String ()

pNumber :: Parser Int
pNumber = (\x -> read x :: Int) <$> many1 digit

pCoord :: Parser Coord
pCoord = do
    n1 <- pNumber 
    char ',' 
    n2 <- pNumber
    pure (n1, n2)

pPath :: Parser [Coord]
pPath = (pCoord `sepBy1` string " -> ")

pScan = pPath `sepBy1` endOfLine

testInput=
    "498,4 -> 498,6 -> 496,6\n\
    \503,4 -> 502,4 -> 502,9 -> 494,9"

testEnded =
    "490,2 -> 505,2"

testSparse="600,1 -> 602,1"