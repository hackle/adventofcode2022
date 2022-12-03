{-# language TypeApplications #-}

module AdventOfCode.Day1 where

import Data.List (sortBy)
import Data.Function ((&))

-- day 1
maxCalorie :: String -> Int
maxCalorie raw = sortBy descending sums & take 3 & sum where
    sums = deers (lines raw) []
    descending x y = compare y x
    deers rest aggr = case rest of
        []      -> aggr
        "":xs   -> deers xs aggr
        _       -> 
            let (xs, ys) = span ("" /=) rest
                summed   = (read @Int <$> xs) & foldl1 (+)
            in deers ys (summed:aggr)
