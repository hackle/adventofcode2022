{-# Language TemplateHaskell #-}
module AdventOfCode.Day24 where

import qualified Data.Map as M
import Control.Lens
import Debug.Trace

type Coord = (Int, Int)
data Grid = Grid {
    _rows :: Int
    , _cols :: Int
    , _minute :: Int
    , _path :: [Coord]
    , _pos :: Coord
    , _wind :: [Int -> Coord]
    }

makeLenses ''Grid

toMap :: Grid -> [Coord]
toMap g = g ^. wind.to (fmap ($ g ^. minute))

instance Show Grid where
    show g = show (toMap g)

toGrid raw =
    let middles = takeMiddle <$> takeMiddle (lines raw)
        xs = mapii (\r c chr -> ((r, c), chr)) middles
        nrows = length xs
        ncols = length $ head xs
        ws = toWind nrows ncols <$> filter (\(_, x) -> x /= '.') (concat xs)
    in Grid nrows ncols 0 [] (-1, 1) ws

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