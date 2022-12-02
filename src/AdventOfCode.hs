{-# language TypeApplications #-}
module AdventOfCode where

import Data.List (sortBy)
import Data.Function ((&))

maxCalorie :: [String] -> Int
maxCalorie raw = sortBy descending sums & take 3 & foldl1 (+) where
    sums = groupsOfString raw []
    descending x y = compare y x
    groupsOfString rest aggr = case rest of
        []      -> aggr
        "":xs   -> groupsOfString xs aggr
        _       -> 
            let (xs, ys) = span ("" /=) rest
                summed   = (read @Int <$> xs) & foldl1 (+)
            in groupsOfString ys (summed:aggr)
