module AdventOfCode.Day25 where

import qualified Data.Map as M
import Data.Bifunctor (bimap, first)
import Debug.Trace
import Data.List (find, nub)
import Data.Maybe (fromMaybe, isJust)
import Data.Function ((&))
import Control.Monad (mplus)
import Data.Tuple (swap)

solve raw = 
    let decs = toDecimal <$> lines raw
    in formatSnafu <$> toSnafu (sum decs)

mappings = M.fromList [('1', 1), ('2', 2), ('0', 0), ('-', -1), ('=', -2)]
reverseMapping = M.fromList $ first show . swap <$> M.toList mappings

-- ghci> toDecimal "1=-0-2"
-- 1747
-- ghci> toDecimal "2=-01"
-- 976
toDecimal raw = sum $ zipWith (\n p -> (mappings M.! n) * (5 ^ p)) raw (reverse [0..length raw - 1])

sumSnafu raw = sum $ toDecimal <$> lines raw

snafuSeq = 0 : concat [[5 ^ n, (5 ^ n) * 2] | n <- [0..]]

findRange :: Int -> [Int]
findRange n =
    let (Just (l, h)) = find (n `between`) (zip snafuSeq (tail snafuSeq))
    in if h == n then [n] else [l, h]
    where between a (b, c) = a >= b && a <= c

-- Int must be product of 5
toSnafuChar :: Int -> Char
toSnafuChar 0 = '0'
toSnafuChar n = reverseMapping M.! show (sign n $ go (abs n))
    where
        go n =
            let b = findPlace n
                k = n `div` (5 ^ b)
            in k

findPlace :: Int -> Int
findPlace n = floor $ logBase 5 (fromIntegral n)
sign x = (*) (if x > 0 then 1 else (-1))

-- xs may miss some places that need backfilling with 0
formatSnafu xs = findOrBackfill <$> reverse [0..findPlace (last xs)]
    where
        findOrBackfill p =
            let nums = [5 ^ p, 5 ^ p * 2, negate (5 ^ p), negate (5 ^ p * 2)]
            in maybe '0' toSnafuChar (find (`elem` xs) nums)


toSnafu n = go [] (abs n) (sign n)
    where
        go :: [Int] -> Int -> (Int -> Int) -> Maybe [Int]
        go done 0 s = Just done
        go done n s =
            let rs = findRange n
                next x = let diff = n - x in go (s x:done) (abs diff) (sign diff . s)
                possibles = [next x | x <- rs, not (placeConflicts done x)]
                stats = "Left: " ++ show n ++ " ranges: " ++ show rs ++ " possibles: " ++ show possibles
            in
                case trace stats possibles of
                    [] -> Nothing
                    _ -> foldl1 mplus possibles

placeConflicts xs candidate =
    let r = abs candidate
    in any (\x -> x `elem` [r, r * 2] || r `elem` [x, x * 2]) (abs <$> xs)

snafus num =
    [
     (   1, "1")
    ,(    2, "2")
    ,(    3, "1=")
    ,(    4, "1-")
    ,(    5, "10")
    ,(    6, "11")
    ,(    7, "12")
    ,(    8, "2=")
    ,(    9, "2-")
    ,(   10, "20")
    ,(   15, "1-0") --15 / 5 = 3, 3 > 2 not representable, must go up, 25 - 15 = 10 == '='
    ,(   20, "1=0") --20 / 5 = 4, 4 > 2 not representable, must go up, 25 - 20 = 5 == '-'
    ,( 2022, "")
    --2022 / 625 = 3, not representable, up to 3125 ('100000') - 2022 = 1103 
    -- 1103 `div` 625 = 1 ('1') + 478 `div` 125 = 3, not representable, back track
    -- 1103 = 2*625 - 1103 = 147, 147 `div` 125 = 1 + 22, 22 `div` 25 = (1), ok + -3, -3 `div` 5 = -1, ok + -2, =
    -- 121--=
    ,(12345, "")
    ,(314159265, "")
    ]


testInput =
    "1=-0-2\n\
    \12111\n\
    \2=0=\n\
    \21\n\
    \2=01\n\
    \111\n\
    \20012\n\
    \112\n\
    \1=-1=\n\
    \1-12\n\
    \12\n\
    \1=\n\
    \122"