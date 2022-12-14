module AdventOfCode.Day12 where

import qualified Data.Matrix as M
import Data.Char
import Data.List (singleton, find, zipWith, partition)
import Data.List.Ordered (insertBagBy)
import Data.Bifunctor (second)
import qualified Data.Map as Mp

type Coord = (Int, Int)

upByMax1 fromN toN = toN - fromN <= 1
downByMax1 = flip upByMax1 --fromN toN = fromN - toN <= 1

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
        -> (Coord -> Bool)
        -> [Coord]   
        -> (BeenTo, [[Coord]]) -- possibility of next steps
pickOne hill beenTo canGo existing@(lastStep:_) = 
    let nexts = filter canGo neighbours 
    in (updateBeenTo nexts beenTo, (: existing) <$> nexts)
    where
        neighbours = findNeighbours lastStep hill

dist (r1, c1) (r2, c2) = (abs $ r2 - r1) + (abs $ c2 - c1)

progress :: M.Matrix Int -> Coord -> ([Coord] -> Bool) 
            -> ([Coord] -> [Coord] -> Ordering) 
            -> (Int -> Int -> Bool) -> (BeenTo, [Coord])
progress hill start toStop ordering pace = 
    let beenTo = M.setElem True start $ const False <$> hill
    in go beenTo [[start]]
    where 
        go :: BeenTo -> [[Coord]] -> (BeenTo, [Coord])
        go beenTo [] = (beenTo, [])
        go beenTo (shortest:possibles) = 
            let canGo x = pace (hill M.! (head shortest)) (hill M.! x) && not (beenTo M.! x)
                step@(beenTo1, news) = pickOne hill beenTo canGo shortest
                (successful, unsuccessful) = partition toStop news
                newPossibles = foldl (insertOrdered ordering) possibles unsuccessful
            in if null successful
                then go beenTo1 newPossibles
                else (beenTo1, head successful)

insertOrdered :: ([Coord] -> [Coord] -> Ordering) -> [[Coord]] -> [Coord] -> [[Coord]]
insertOrdered ordering st cur = insertBagBy ordering cur st

orderByLength hill (x:xs) (y:ys) = (length xs) `compare` (length ys)

orderByHeight hill (x:xs) (y:ys) = (hill M.! y) `compare` (hill M.! x)    -- lower first

orderThen :: (a -> a -> Ordering) -> (a -> a -> Ordering) -> (a -> a -> Ordering)
orderThen f g x y = f x y <> g x y

visualised :: M.Matrix Int -> [Coord] -> M.Matrix String
visualised hill shortest = 
    let numberedPath = Mp.fromList $ zipWith (,) shortest [0..]
    in M.mapPos (\pos c -> maybe [' ', chr c, ' '] (const $ "[" ++ [chr c] ++ "]") $ Mp.lookup pos numberedPath) hill

runApp :: (Coord, Coord, String) -> [(M.Matrix String, Int)]
runApp (start, end, raw) = 
    let hill = ord <$> (M.fromLists $ lines raw)
        (_, shortest1) = progress hill start (\(h:_) -> h == end) (orderByLength hill) upByMax1
        (_, shortest2) = progress hill end (\(h:_) -> hill M.! h == ord 'a') (orderThen (orderByHeight hill) (orderByLength hill)) downByMax1
    in (\p -> (visualised hill p, length p - 1)) <$> [shortest1, shortest2]

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