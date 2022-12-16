module AdventOfCode.Day15 where

import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad

type Coord = (Int, Int)
type Range = (Int, Int)
type SB = (Coord, Coord)
type SensorRange = (Coord, Int)

-- allTaken 7 [((8,7), 9)]
-- ghci> allTaken (-3) [((8,7), 9)]
-- fromList []
-- ghci> allTaken (-2) [((8,7), 9)]
-- fromList [8]
-- ghci> allTaken (16) [((8,7), 9)]
-- fromList [8]
-- ghci> allTaken (17) [((8,7), 9)]
-- fromList []
-- ghci> allTaken (20) [((8,7), 9)]
-- fromList []

covered :: Int -> [SensorRange] -> S.Set Int
covered lineNo ranges = S.unions expanded
    where
        map1 :: SensorRange -> S.Set Int
        map1 sr = let mapped = mapTo lineNo sr in maybe S.empty expand mapped
        expanded = map1 <$> ranges

expand :: Range -> S.Set Int
expand (x1, x2) = S.fromList [x1..x2]

mapTo :: Int -> SensorRange -> Maybe Range
mapTo lineNo range@((x, y), radius) = 
    if mappedRadius >= 0 
        then Just (x - mappedRadius, x + mappedRadius)
        else Nothing
    where
        heightDiff = abs (y - lineNo)
        mappedRadius = radius - heightDiff

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

sbToSensorRange :: SB -> SensorRange
sbToSensorRange (sensor, beacon) = (sensor, manhattan sensor beacon) 

isInSb :: Coord -> SensorRange -> Bool
isInSb point (beacon, radius) = (dist point beacon) <= radius

dist (x1, y1) (x2, y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

edges :: SensorRange -> [Coord]
edges sr@((x, y), w) = [(x + b * (w - abs y1), y + y1) | y1 <- [(0 - w) .. w], b <- [1, -1]]

findFree :: [Coord] -> [SensorRange] -> Maybe Coord
findFree pts ranges = L.find (not . inAnyRange) pts
    where inAnyRange pt = any id (fmap (isInSb pt) ranges)

findInRange1 :: [SensorRange] -> Coord -> SensorRange -> Maybe Coord
findInRange1 ranges (mn, mx) (coord, w) =
    let edgePoints = edges (coord, w + 1)
        inRange = filter (\(x, y) -> x >= mn && x <= mx && y >= mn && y <= mx) edgePoints
    in findFree inRange ranges

freeSpot :: [SB] -> Range -> Maybe Coord
freeSpot sbs coordRange =  
    let allRanges = sbToSensorRange <$> sbs
        frees = findInRange1 allRanges coordRange <$> allRanges
    in foldl1 mplus frees

round2 sbs = showIt <$> freeSpot sbs (0, 4000000) 
    where showIt found@(x, y) = (found, x * 4000000 + y)

round1 sbs lineNo = (coveredOnLine `S.difference` sittingOnLine)
    where
        coveredOnLine = covered lineNo (sbToSensorRange <$> sbs)
        sittingOnLine = let onlines = filter isOnline sbs in S.fromList $ fmap (fst . snd) onlines
        isOnline :: SB -> Bool
        isOnline (_, (_, y)) = y == lineNo

testInput :: [SB]
testInput = [
    ((2, 18), (-2, 15)),
    ((9, 16), (10, 16)),
    ((13, 2), (15, 3)),
    ((12, 14), (10, 16)),
    ((10, 20), (10, 16)),
    ((14, 17), (10, 16)),
    ((8, 7), (2, 10)),
    ((2, 0), (2, 10)),
    ((0, 11), (2, 10)),
    ((20, 14), (25, 17)),
    ((17, 20), (21, 22)),
    ((16, 7), (15, 3)),
    ((14, 3), (15, 3)),
    ((20, 1), (15, 3))
    ]

trueInput :: [SB]
trueInput = [
    ((3428425, 2345067), (3431988, 2379841)),
    ((928237, 25774), (1212315, -161555)),
    ((2061220, 2396791), (2038311, 2495160)),
    ((1830400, 2994568), (1910058, 3117415)),
    ((2485733, 2625804), (2038311, 2495160)),
    ((1855873, 3971916), (1910058, 3117415)),
    ((119582, 3929652), (311197, 4221202)),
    ((1069031, 3509672), (1910058, 3117415)),
    ((3368023, 2213635), (3431988, 2379841)),
    ((3713877, 2460862), (3431988, 2379841)),
    ((3593503, 2174008), (3507689, 2000000)),
    ((501760, 93436), (1212315, -161555)),
    ((3712703, 214999), (3507689, 2000000)),
    ((1594824, 2790273), (1910058, 3117415)),
    ((2539549, 3190814), (1910058, 3117415)),
    ((3522790, 2671548), (3431988, 2379841)),
    ((1001452, 1327490), (1212315, -161555)),
    ((629209, 2451628), (-416149, 2226089)),
    ((2636827, 1146266), (3507689, 2000000)),
    ((3909, 625124), (1212315, -161555)),
    ((3950231, 3688780), (3888160, 3226725)),
    ((3449978, 2328058), (3431988, 2379841)),
    ((3974214, 2582925), (3888160, 3226725)),
    ((82663, 3225533), (311197, 4221202)),
    ((1958305, 2292045), (2038311, 2495160)),
    ((3465738, 2123353), (3507689, 2000000)),
    ((2940758, 3884337), (2746166, 4800483)),
    ((3429173, 2275591), (3431988, 2379841)),
    ((1527349, 38565), (1212315, -161555)),
    ((3049925, 2498038), (3431988, 2379841)),
    ((1593202, 3335178), (1910058, 3117415)),
    ((3175520, 3230234), (3888160, 3226725))
    ]