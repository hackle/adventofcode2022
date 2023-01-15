module AdventOfCode.Array where

-- import qualified Data.Map as M
import qualified Data.List as L
import Data.Foldable
import qualified Data.Map as M
import Data.Function ((&))

arrayManipulation :: Int -> [[Int]] -> Int
arrayManipulation n queries = fst $ M.foldl comp (0, 0) keyed
    where 
        comp (mx, agg) f = let agg1 = f agg in (max mx agg1, agg1)
        keyed = keyedFns <$> queries & 
                concat &
                M.fromListWith (.)
        keyedFns [a, b, v] = [(a, (+ v)), (b + 1, (\x -> x - v))]

try1 n queries = fst $ foldl comp (0, 0) keyed
    where 
        comp (mx, agg) (_, fn) = let agg1 = fn agg in (max mx agg1, agg1)
        keyed = L.sortOn fst $ concat $ keyedFns <$> queries
        keyedFns [f, t, v] = [(f, (+ v)), (t + 1, (\x -> x - v))]
    
testInput2 :: [[Int]]
testInput2 = [
    [ 5, 6, 8 ]
    ,[ 3, 5, 7 ]
    ,[ 5, 8, 1 ]
    ,[ 5, 9, 15 ]
    , [2, 4, 7]
    ]

testInput1 :: [[Int]]
testInput1 = [
    [1, 2, 100]
    , [2, 5, 100]
    , [3, 4, 100]
    ]

testInput :: [[Int]]
testInput = [
    [1, 5, 3]
    , [4, 8, 7]
    , [6, 9, 1]
    ]