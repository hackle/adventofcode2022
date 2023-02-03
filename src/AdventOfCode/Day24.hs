{-# Language TemplateHaskell #-}
module AdventOfCode.Day24 where

import qualified Data.Map as M
import Control.Lens
import Debug.Trace
import Data.List.Ordered (insertBagBy, sortBy, unionBy)
import qualified Data.Set as S
import Data.Function ((&))
import Data.List (partition)
import Data.Maybe (isJust)
import Data.Matrix (matrix)

type Coord = (Int, Int)
data Grid = Grid {
    _rows :: Int
    , _cols :: Int
    , _wind :: [Int -> Coord]
    }

makeLenses ''Grid

toMap :: Int -> Grid -> [Coord]
toMap m g = g ^. wind.to (fmap ($ m))

winds g = 
    let mx = g ^. rows * g ^. cols in M.fromList [(n, toMap n g) | n <- [0..mx]]

instance Show Grid where
    show g = show (toMap 0 g)

neighbours (r, c) = [(r + r1, c + c1) | r1 <- [-1..1], c1 <- [-1..1], (r1, c1) /= (0, 0) && abs (r1 - c1) == 1 ]

shortest g = go 0 [[(-1, 0)]] [] S.empty
    where
        ws = winds g
        go n [] (s:_) beenTo = s
        go n [] [] _ = error $ show $ showG [(0, 0)] g
        go n xss done beenTo =
            let (xs, rest) = trace (show $ head xss) $ splitAt 2 xss
                xs1 = concatMap (`proceed` beenTo) xs
                (done1, toGo1) = partition (isDone g) xs1
                done2 = unionBy sorting done1 done
                trimmer = if null done2 then id else filter (hopeful (traceShowId $ head done2) g)
                rest1 = trimmer $ unionBy sorting toGo1 rest
            in go (n + 1) rest1 done2 (beenTo `S.union` S.fromList (head <$> xs1))
        sorting = closesToEnd g
        proceed _xs@(x:xs) beenTo =
            let w = ws M.! (length _xs `mod` length ws)
                ns = filter (\x1 -> inRange g x1 && x1 `notElem` w) (x : neighbours x)
                attempts = (: _xs) <$> ns
            in sortBy sorting attempts

showG [] g = []
showG _xs@(x:xs) g =
    matrix (g ^. rows) (g ^. cols) getter : showG xs g
    where
        numbered = (\c -> (c, 1)) <$> toMap (length _xs - 1) g
        mp = M.fromListWith (+) numbered
        getter (r, c) = 
            case (r - 1, c - 1) `M.lookup` mp of
                Nothing -> if x == (r - 1, c - 1) then "E" else "."
                Just cnt -> show cnt

inRange g (r, c) = (r, c) == (-1, 0) || (r >= 0 && c >= 0 && r < g ^. rows && c < g ^. cols)

hopeful best g candidate@(x:_) = 
    let e = endCoord g
    in length candidate + manhattan x e < length best  -- best scenario can beat best, worth pursuing

endCoord g = (g ^. rows, g ^. cols - 1)
manhattan (r1, c1) (r2, c2) = r2 - r1 + c2 - c1

repeats [] = 0
repeats (z:zs) = length $ takeWhile (== z) zs

closesToEnd g xs@(x:_) ys@(y:_) =
    let e = endCoord g
    in manhattan x e `compare` manhattan y e <> length xs `compare` length ys

isDone g (x:_) = 1 == manhattan x (endCoord g)


toGrid raw =
    let middles = takeMiddle <$> takeMiddle (lines raw)
        xs = mapii (\r c chr -> ((r, c), chr)) middles
        nrows = length xs
        ncols = length $ head xs
        ws = toWind nrows ncols <$> filter (\(_, x) -> x /= '.') (concat xs)
    in Grid nrows ncols ws

toWind rs cs ((r, c), chr) m =
    case chr of
        '>' -> (r, (c + m) `mod` cs)
        '<' -> (r, (c - m) `mod` cs)
        '^' -> ((r - m) `mod` rs, c)
        'v' -> ((r + m) `mod` rs, c)


mapi :: (Int -> a -> c) -> [a] -> [c]
mapi fn = zipWith fn [0..]

mapii :: (Int -> Int -> a -> c) -> [[a]] -> [[c]]
mapii f = mapi (mapi . f)

takeMiddle (_:xs) = take (length xs - 1) xs


testInput =
    "#.######\n\
    \#>>.<^<#\n\
    \#.<..<<#\n\
    \#>v.><>#\n\
    \#<^v^^>#\n\
    \######.#"