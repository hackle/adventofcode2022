module AdventOfCode.Day6 where

import Data.List

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
startOfPacket xs = go rest mLen taken
    where
        (taken, rest) = splitAt mLen xs
        mLen = 14
        go :: [Char] -> Int -> [Char] -> Maybe Int
        go [] _ _ = Nothing
        go rest@(a:as) pos taken@(b:c:bs) = 
            if length (nub taken) == mLen 
                then Just pos
                else go as (pos + 1) (bs++[a])