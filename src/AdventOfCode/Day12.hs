module AdventOfCode.Day12 where

import qualified Data.Matrix as M
import Data.Char
import Data.List (singleton, find, zipWith, partition)
import Data.List.Ordered (insertBagBy)
import Data.Bifunctor (second)
import qualified Data.Map as Mp

hill :: String -> M.Matrix Int
hill raw = ord <$> (M.fromLists $ lines raw)

type Coord = (Int, Int)

climbable fromN toN = toN - fromN `elem` [1,0,-1,-2]

neighbourCoords = [
            (-1, 0),
    (0, -1),       (0, 1),
            (1, 0) 
    ]

findNeighbours :: Coord -> M.Matrix Int -> [Coord]
findNeighbours cur hill = foldl getOne [] (add cur <$> neighbourCoords)
    where
        add (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)
        getOne aggr coord@(r, c) = let res = M.safeGet r c hill in maybe aggr (const $ coord : aggr) res

type BeenTo = M.Matrix Bool

updateBeenTo :: [Coord] -> BeenTo -> BeenTo
updateBeenTo coords beenTo = foldl (\bt c -> M.setElem True c bt) beenTo coords

pickOne :: M.Matrix Int -- hill
        -> BeenTo  -- all been to
        -> [Coord]   
        -> (BeenTo, [[Coord]]) -- possibility of next steps
pickOne hill beenTo existing@(lastStep:_) = 
    let nexts = filter canGo neighbours 
    in (updateBeenTo nexts beenTo, (: existing) <$> nexts)
    where
        canGo x = climbable (hill M.! lastStep) (hill M.! x) && not (beenTo M.! x)
        neighbours = findNeighbours lastStep hill

insertOrdered :: Coord -> M.Matrix Int -> [[Coord]] -> [Coord] -> [[Coord]]
insertOrdered hill stop st cur = insertBagBy (ordering stop hill) cur st

ordering hill stop (x:xs) (y:ys) = 
    let lenComp = (length xs) `compare` (length ys)
        heightComp = (hill M.! y) `compare` (hill M.! x)    -- reversed, higher than than lower
        distComp = dist x stop `compare` dist y stop
    in case (heightComp, lenComp, distComp) of
        (EQ, EQ, _) -> distComp
        (EQ, _, _) -> lenComp
        _ -> heightComp

distTo :: Coord -> Coord -> Coord -> Ordering
distTo stop c1 c2 = compare (dist c1 stop) (dist c2 stop)

dist (r1, c1) (r2, c2) = (abs $ r2 - r1) + (abs $ c2 - c1)

progress :: M.Matrix Int -> Coord -> Coord -> (BeenTo, [Coord])
progress hill start stop = 
    let beenTo = M.setElem True start $ const False <$> hill 
    in go beenTo [[start]]
    where 
        go :: BeenTo -> [[Coord]] -> (BeenTo, [Coord])
        go beenTo [] = (beenTo, [])
        go beenTo (shortest:possibles) = 
            let step@(beenTo1, news) = pickOne hill beenTo shortest
                (successful, unsuccessful) = partition (\(h:_) -> dist h stop == 0) news
                newPossibles = foldl (insertOrdered stop hill) possibles unsuccessful
            in if null successful
                then go beenTo1 newPossibles
                else (beenTo1, head successful)


runApp :: (Coord, Coord, String) -> (M.Matrix String, Int)
runApp (start, end, raw) = 
    let h = hill raw 
        (beenTo, shortest) = progress h start end
        numbered = Mp.fromList $ zipWith (,) shortest [0..]
    in (M.mapPos (\pos c -> maybe [' ', chr c, ' '] (const $ "[" ++ [chr c] ++ "]") $ Mp.lookup pos numbered) h, length shortest - 1)

-- parseInput :: String -> (Coord, Coord, M.Matrix Int)
-- parseInput raw = 
--     let m = M.fromLists $ lines raw

testInput :: ((Int, Int), (Int, Int), String)
testInput = ((1, 1), (3, 6),
    "aabqponm\n\
    \abcryxxl\n\
    \accszzxk\n\
    \acctuvwj\n\
    \abdefghi")

trueInput :: ((Int, Int), (Int, Int), String)
trueInput = ((21, 1), (21, 37), 
    "abccccaaaaaaacccaaaaaaaccccccccccccccccccccccccccccccccccaaaa\n\
    \abcccccaaaaaacccaaaaaaaaaaccccccccccccccccccccccccccccccaaaaa\n\
    \abccaaaaaaaaccaaaaaaaaaaaaaccccccccccccccccccccccccccccaaaaaa\n\
    \abccaaaaaaaaaaaaaaaaaaaaaaacccccccccaaaccccacccccccccccaaacaa\n\
    \abaccaaaaaaaaaaaaaaaaaacacacccccccccaaacccaaaccccccccccccccaa\n\
    \abaccccaaaaaaaaaaaaaaaacccccccccccccaaaaaaaaaccccccccccccccaa\n\
    \abaccccaacccccccccaaaaaacccccccccccccaaaaaaaacccccccccccccccc\n\
    \abcccccaaaacccccccaaaaaaccccccccijjjjjjaaaaaccccccaaccaaccccc\n\
    \abccccccaaaaacccccaaaacccccccciiijjjjjjjjjkkkkkkccaaaaaaccccc\n\
    \abcccccaaaaacccccccccccccccccciiiirrrjjjjjkkkkkkkkaaaaaaccccc\n\
    \abcccccaaaaaccccccccccccccccciiiirrrrrrjjjkkkkkkkkkaaaaaccccc\n\
    \abaaccacaaaaacccccccccccccccciiiqrrrrrrrrrrssssskkkkaaaaacccc\n\
    \abaaaaacaaccccccccccccccccccciiiqqrtuurrrrrsssssskklaaaaacccc\n\
    \abaaaaacccccccccccaaccccccccciiqqqttuuuurrssusssslllaaccccccc\n\
    \abaaaaaccccccccaaaaccccccccciiiqqqttuuuuuuuuuuusslllaaccccccc\n\
    \abaaaaaacccccccaaaaaaccccccciiiqqqttxxxuuuuuuuusslllccccccccc\n\
    \abaaaaaaccccaaccaaaaacccccchhiiqqtttxxxxuyyyyvvsslllccccccccc\n\
    \abaaacacccccaacaaaaaccccccchhhqqqqttxxxxxyyyyvvsslllccccccccc\n\
    \abaaacccccccaaaaaaaacccccchhhqqqqtttxxxxxyyyvvssqlllccccccccc\n\
    \abacccccaaaaaaaaaaccaaacchhhpqqqtttxxxxxyyyyvvqqqlllccccccccc\n\
    \abaaacaaaaaaaaaaaacaaaaahhhhppttttxxzzzzzyyvvvqqqqlllcccccccc\n\
    \abaaaaaaacaaaaaacccaaaaahhhppptttxxxxxyyyyyyyvvqqqlllcccccccc\n\
    \abaaaaaaccaaaaaaaccaaaaahhhppptttxxxxywyyyyyyvvvqqqmmcccccccc\n\
    \abaaaaaaacaaaaaaacccaaaahhhpppsssxxwwwyyyyyyvvvvqqqmmmccccccc\n\
    \abaaaaaaaaaaaaaaacccaacahhhpppssssssswyyywwvvvvvqqqmmmccccccc\n\
    \abaaaaaaaacacaaaacccccccgggppppsssssswwywwwwvvvqqqqmmmccccccc\n\
    \abcaaacaaaccccaaaccccccccgggppppppssswwwwwrrrrrqqqmmmmccccccc\n\
    \abcaaacccccccccccccccccccgggggpppoosswwwwwrrrrrqqmmmmddcccccc\n\
    \abccaacccccccccccccccccccccgggggoooosswwwrrrnnnmmmmmddddccccc\n\
    \abccccccccccccccccccccccccccgggggooossrrrrrnnnnnmmmddddaccccc\n\
    \abaccccaacccccccccccccccccccccgggfoossrrrrnnnnndddddddaaacccc\n\
    \abaccaaaaaaccccccccccccccccccccgffooorrrrnnnneeddddddaaaacccc\n\
    \abaccaaaaaacccccccccccccccccccccfffooooonnnneeeddddaaaacccccc\n\
    \abacccaaaaaccccccccaaccaaaccccccffffoooonnneeeeccaaaaaacccccc\n\
    \abcccaaaaacccccccccaaccaaaaccccccffffoooneeeeeaccccccaacccccc\n\
    \abaccaaaaaccccccccaaaacaaaaccccccafffffeeeeeaaacccccccccccccc\n\
    \abacccccccccccccccaaaacaaacccccccccffffeeeecccccccccccccccaac\n\
    \abaaaacccccccaaaaaaaaaaaaaacccccccccfffeeeccccccccccccccccaaa\n\
    \abaaaacccccccaaaaaaaaaaaaaaccccccccccccaacccccccccccccccccaaa\n\
    \abaacccccccccaaaaaaaaaaaaaaccccccccccccaacccccccccccccccaaaaa\n\
    \abaaaccccccccccaaaaaaaaccccccccccccccccccccccccccccccccaaaaaa")