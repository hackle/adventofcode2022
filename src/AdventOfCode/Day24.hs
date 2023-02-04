{-# Language TemplateHaskell #-}
module AdventOfCode.Day24 where

import qualified Data.Map as M
import Control.Lens
import Debug.Trace
import Data.List.Ordered (insertBagBy, sortBy)
import qualified Data.Set as S
import Data.Function ((&))
import Data.List (partition, sortBy, minimumBy)
import Data.Maybe (isJust)
import Data.Matrix (matrix)
import Control.Parallel
import Control.Parallel.Strategies

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
    let mx = g ^. rows * g ^. cols
        maps = [(n, toMap n g) | n <- [0..mx]]
    in M.fromList maps

instance Show Grid where
    show g = show (toMap 0 g)

neighbours (r, c) = (r, c) : [(r + r1, c + c1) | r1 <- [-1..1], c1 <- [-1..1], abs (r1 - c1) == 1 ]


shortest g = foldl (\n (s, e) -> 1 + go n (S.fromList [s]) e) 0 rounds
    where
        rounds = [(startTopLeft, endBottomRight g), (startBottomRight g, endTopLeft), (startTopLeft, endBottomRight g)]
        ws = winds g
        e = endBottomRight g
        go n visited end =
            let w = ws M.! (n `mod` length ws)
                nexts = concatMap (proceed w) (S.toList visited)
                xs1 = S.fromList (nexts `using` parTraversable rdeepseq)
                stats = show n ++ ":" ++ show (length xs1)
            in 
                trace ("Continuing " ++ stats) $
                if S.null $ S.filter (== end) xs1 
                    then go (n + 1) xs1 end 
                    else trace (">>>>>>>Success<<<<< " ++ show n) n
        proceed w x =
            let ns = filter (\x1 -> inRange g x1 && x1 `notElem` w) (x : neighbours x)
            in ns

inRange g (r, c) = (r, c) `elem` [ startTopLeft, startBottomRight g ] || (r >= 0 && c >= 0 && r < g ^. rows && c < g ^. cols)

endBottomRight g = (g ^. rows - 1, g ^. cols - 1)
endTopLeft = (0, 0)

startTopLeft = (-1, 0)
startBottomRight g = let (r, c) = endBottomRight g in (r + 1, c)

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


