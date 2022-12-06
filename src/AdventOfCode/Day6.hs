module AdventOfCode.Day6 where

import qualified Data.Vector as V
import qualified Data.List as L

-- 
-- ghci> startOfPacket "bvwbjplbgvbhsrlpgdmjqwftvncz"
-- Just 5
-- ghci> startOfPacket "nppdvjthqldpwncqszvftbrmjlhg"
-- Just 6
-- ghci> startOfPacket "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
-- Just 10
-- ghci> startOfPacket "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
-- Just 11
startOfPacket :: String -> Maybe Int
startOfPacket xs = (+ 14) . fst <$> V.find isPacket indexedSubs
    where
        mLen = 14
        vs = V.fromList xs
        indexedSubs = fmap (\(idx, a) -> (idx, V.slice idx mLen vs)) (V.indexed vs)
        isPacket (idx, v) = (L.length $ L.nub $ V.toList v) == mLen