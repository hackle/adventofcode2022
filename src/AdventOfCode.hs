{-# language TypeApplications #-}
module AdventOfCode where

import Data.List (sortBy)
import Data.Function ((&))

maxCalorie :: [String] -> Int
maxCalorie raw = sortBy descending sums & take 3 & foldl1 (+) where
    sums = deers raw []
    descending x y = compare y x
    deers rest aggr = case rest of
        []      -> aggr
        "":xs   -> deers xs aggr
        _       -> 
            let (xs, ys) = span ("" /=) rest
                summed   = (read @Int <$> xs) & foldl1 (+)
            in deers ys (summed:aggr)
