{-# Language NamedFieldPuns #-}

module AdventOfCode.Day8 where

import qualified Data.Matrix as M
import Data.Bifunctor
import qualified Data.Vector as V

treeHouse :: String -> (Int, Int)
treeHouse raw = (total, topScore)
    where 
        mx = toMatrix (lines raw)
        topScore = highestScore mx
        visibles = toVisible mx
        total = foldl (\c v -> if v then c + 1 else c) 0 visibles

highestScore :: M.Matrix Int -> Int
highestScore mx = foldl max 0 (M.mapPos (scoreAt mx) mx)

directional mx r c = 
    let (lefts, rights) = V.splitAt (c - 1) (M.getRow r mx) -- matrix is 1 based!
        (tops, downs) = V.splitAt (r - 1) (M.getCol c mx)
    in V.fromList [V.reverse lefts, V.drop 1 rights, V.reverse tops, V.drop 1 downs]

scoreAt :: M.Matrix Int -> (Int, Int) -> Int -> Int
scoreAt mx coords@(r, c) height = 
    let directed = directional mx r c
        isInView (aggr, goOn) v = if goOn then (aggr + 1, v < height) else (aggr, False)
        inView = fst . V.foldl isInView (0, True) <$> directed
    in  V.foldl (*) 1 inView

toVisible :: M.Matrix Int -> M.Matrix Bool
toVisible tree = M.mapPos (\coords v -> isVisible v coords tree) tree
    where inside = M.submatrix 2 (M.nrows tree - 1) 2 (M.ncols tree - 1) tree

isVisible :: Int -> (Int, Int) -> M.Matrix Int -> Bool 
isVisible val coords@(r, c) mx = visibleInLine (c - 1) sameRow || visibleInLine (r - 1) sameCol
    where
        sameRow = M.getRow r mx
        sameCol = M.getCol c mx
        allLower = V.all (< val)
        visibleInLine idx ln = 
            let (v1, _v2) = V.splitAt idx ln
                v2 = V.drop 1 _v2
            in allLower v1 || allLower v2

toMatrix :: [[Char]] -> M.Matrix Int
toMatrix xs = (\x -> read [x] :: Int) <$> M.fromLists xs

testInput = "30373\n\
\25512\n\
\65332\n\
\33549\n\
\35390"