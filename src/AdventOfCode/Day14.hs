module AdventOfCode.Day14 where

import qualified Data.Map as M
import Text.Parsec
import Control.Monad

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

starting = (500, 0)
fallOrder = [(0, 1), (-1, 1), (1, 1)]

addCoord (c1, r1) (c2, r2) = (c1 + c2, r1 + r2)

lowest :: Bricks -> Int
lowest = fst . fst . M.findMax  -- col increase downwards

fall1 :: 
        Int -- lowest row of bricks
        -> Bricks
        -> Maybe Coord   -- success or failure both in coords
fall1 lowest bricks = go starting
    where 
        go pos@(_, r) =
            if r > lowest       -- lowest is actually the biggest number
                then Nothing    -- falling to the void
                else maybe (Just pos) go (msum $ moves pos)   -- Nothing for all blocked, or keep going
        moves pos = move1 pos <$> fallOrder
        move1 pos offset = 
            let pos1 = addCoord offset pos 
            in maybe (Just pos1) (const Nothing) (pos1 `M.lookup` bricks) -- if pos is taken, fail 
    
fall :: Bricks
        -> (Int, Bricks)    -- count of sands till it fails
fall bricks = go 0 bricks
    where
        lowestRow = lowest bricks
        go cnt bricks = 
            case fall1 lowestRow bricks of
                Nothing -> (cnt, bricks)
                Just pos -> go (cnt + 1) (M.insert pos 'o' bricks)

runApp raw =
    case parse pScan "" raw of
        Left err -> show err
        Right paths ->
            let bricks = toBricks paths
                fallen = fall bricks
            in show fallen

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