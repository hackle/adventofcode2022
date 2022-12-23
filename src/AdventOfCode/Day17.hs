{-# Language NamedFieldPuns #-}

module AdventOfCode.Day17 where

import qualified Data.Matrix as M
import qualified Data.Map as Mp
import Data.Bifunctor
import Debug.Trace
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.List as L
import Data.List.Ordered (insertBagBy)
import Control.Monad

-- trace _ v = v
-- traceShowId = id

type Coord = (Int, Int)

horiline :: [Coord]
horiline = [ (1, 3), (1, 4), (1, 5), (1, 6) ]

vertiline :: [Coord]
vertiline = [ 
    (4, 3),
    (3, 3),
    (2, 3), 
    (1, 3)
    ]

cross :: [Coord]
cross = [ 
            (3, 4), 
    (2, 3), (2, 4), (2, 5), 
            (1, 4) 
        ]

club :: [Coord]
club = [
                    (3, 5),
                    (2, 5),
    (1, 3), (1, 4), (1, 5)
    ]

brick :: [Coord]
brick = [
    (2, 3), (2, 4),
    (1, 3), (1, 4)
    ]

type Jet = [Char]

type Tower = M.Matrix Char
type Block = M.Matrix Char

type Shape = [Coord]
-- data Shape =
--     Shape {
--         position :: Coord,
--         rock :: [Coord]
--     } deriving (Eq, Show)

-- ghci> jet '>' horiline
-- [(1,4),(1,5),(1,6),(1,7)]
-- ghci> (jet '<' $ jet '>' horiline) == horiline
-- True
jet :: Char -> Shape -> Shape
jet direction shape = 
    let jetted = sequence $ (op direction) <$> shape
    in maybe shape id jetted

-- ghci> op '>' (1, 3)
-- Just (1,4)
-- ghci> op '<' (1, 3)
-- Just (1,2)
op :: Char -> Coord -> Maybe Coord
op ch (r, c) = 
    let c1 = op c
    in if c1 < 1 || c1 > 7 
        then Nothing
        else Just (r, c1)
    where op c = if ch == '>' then (c + 1) else (c - 1)

-- ghci> inRange (newTower 3) (positionShape 2 horiline)
-- True
-- ghci> inRange (newTower 2) (positionShape 2 horiline)
-- True
-- ghci> inRange (newTower 2) (positionShape 1 horiline)
-- False
inRange :: Tower -> Shape -> Bool
inRange tower shape = 
    let tried = sequence $ (\(r, c) -> (== '.') <$> (M.safeGet r c tower)) <$> shape
    in maybe False (all id) tried 

-- ghci> sTower $ fallBy1 (initialState jetPatterns) (positionShape 4 horiline)
-- │ '.' '.' '#' '#' '#' '#' '.' │
-- │ '.' '.' '.' '.' '.' '.' '.' │
-- │ '.' '.' '.' '.' '.' '.' '.' │
-- └                             ┘
fallBy1 :: State -> Shape -> State
fallBy1 st@State{ sJet = (j:js), sTower, sTop } shape = 
    let jetted = let try = jet j shape in if inRange sTower try then try else shape
        dropped = dropBy 1 jetted
    in if inRange sTower dropped
        then fallBy1 st{sJet=js} dropped
        else truncateState st{ sTower = rest sTower jetted, sJet=js, sTop = max sTop (maximum $ fst <$> jetted) }

truncateState :: State -> State
truncateState st@State{sTower, sTop, sMemoTop, sHeight} =
    let (yOffset, truncatedTower) = tryTruncate sTower
    in st{sTower = truncatedTower, sTop = sTop - yOffset, sHeight = sHeight - yOffset, sMemoTop = sMemoTop + fromIntegral yOffset }

-- ghci> tryTruncate ((const '#' <$> newTower 4) <-> newTower 3)

tryTruncate :: Tower -> (Int, Tower)    -- int for deleted rows
tryTruncate tower = 
    let maxRow = M.nrows tower
        paths = map1 <$> (V.reverse $ V.filter (\(_, c) -> c == '#') $  V.indexed (M.getCol 1 tower))
        everyColHasData = all id ((\c -> V.elem '#' $ M.getCol c tower) <$>[2..7])
    in if not everyColHasData then (0, tower) else
        case foldl mplus Nothing paths of
            Nothing -> (0, tower)
            Just valid -> 
                let bottom = minimum $ fst <$>  valid
                in (bottom - 1, M.submatrix bottom maxRow 1 7 tower)
    where
        map1 (idx, c) = if c == '#' then shortest tower (idx + 1, 1) else Nothing -- vector is 0 based, matrix 1 

dropBy :: Int -> Shape -> Shape
dropBy n = fmap (\(r, c) -> (r - n, c))

rest :: Tower -> Shape -> Tower
rest tower shape = foldl (\t c -> M.setElem '#' c t) tower shape

data State = State { 
    sJet :: Jet, 
    sTower :: Tower, 
    sBase :: Int,   -- the floating base of the tower, it's not possible to fall through the base
    sHeight :: Int, 
    sTop :: Int,
    sMemoTop :: Integer, -- memoised top with truncation in consideration
    sMemo :: Mp.Map String (Integer, Integer)  -- cached state for checking comparison (round, top of tower)
    } deriving (Eq, Show)

minSpace :: Int
minSpace = 3

-- height will be yTop + yOffset
newTower height = M.matrix height 7 (const '.')

initialState :: Jet -> State
initialState jets = 
    State { 
        sJet = jets, 
        sTower = newTower minSpace, 
        sHeight = minSpace, 
        sTop = 0, 
        sBase = 0, 
        sMemoTop = 0,
        sMemo = Mp.empty
        }

-- ghci> shortest ((const '#' <$> newTower 4) <-> newTower 3) (1, 1)
-- Just [(4,7),(4,6),(4,5),(4,4),(3,3),(2,2),(1,1)]
-- ghci> shortest ((const '#' <$> newTower 4) <-> newTower 3) (2, 1)
-- Just [(4,7),(4,6),(4,5),(4,4),(4,3),(3,2),(2,1)]
-- ghci> shortest ((const '#' <$> newTower 4) <-> newTower 3) (3, 1)
-- Just [(4,7),(4,6),(4,5),(4,4),(4,3),(4,2),(3,1)]

-- ghci> shortest ((const '#' <$> newTower 4) <-> newTower 3) (4, 1)
-- Just [(4,7),(4,6),(4,5),(4,4),(4,3),(4,2),(4,1)]
-- ghci> shortest ((const '#' <$> newTower 4) <-> newTower 3) (5, 1)
-- Just [(4,7),(4,6),(4,5),(4,4),(4,3),(4,2),(5,1)]
-- ghci> shortest ((const '#' <$> newTower 4) <-> newTower 3) (6, 1)
-- Nothing
shortest :: Tower -> Coord -> Maybe [Coord]
shortest tower from = go [[from]] (S.singleton from)
    where
        go :: [[Coord]] -> S.Set Coord -> Maybe [Coord]
        go [] _ = Nothing
        go (best@((_, c):_):rest) beenTo = 
            if c == 7 then Just best else
                case (branchOut beenTo best tower) of
                    ([], beenToMore) -> go rest beenToMore
                    (branches, beenToMore) -> 
                        let updatedQueue = foldl (\agg cur -> insertBagBy closerToRight cur agg) rest branches
                        in go updatedQueue beenToMore

neighbourCoords = [(r, c) | r <- [-1, 0, 1], c <- [-1, 0, 1], (r, c) /= (0, 0)]
addCoords (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

branchOut :: S.Set Coord -> [Coord] -> Tower -> ([[Coord]], S.Set Coord)
branchOut beenTo best@(h:_) tower =
    (getAt <$> neighbours, S.union beenTo (S.fromList neighbours))
     where
        neighbours = filter (\c -> existsAt c && not (S.member c beenTo)) (addCoords h <$> neighbourCoords)
        existsAt coord = maybe False (\c -> c == '#') $ uncurry M.safeGet coord tower
        getAt coord = coord:best

closerToRight cs1@((_, c1):_) cs2@((_, c2):_) = c2 `compare` c1 -- <> L.length cs1 `compare` L.length cs2 -- higher the number the closer to the right, the better

positionShape :: Int -> Shape -> Shape
positionShape yBase shape =
    let yOffset = yBase - 1
    in (first (\y -> y + yOffset)) <$> shape

-- getRockTop :: Tower -> Int
-- getRockTop tower = 
--     let maybeMax = M.lookupMax $ M.filter (\x -> x == '#') tower
--     in maybe 0 (snd . fst) maybeMax

shapeHeight shape = 
    let ys = fst <$> shape
    in maximum ys - minimum ys + 1 -- inclusive

data Repeat = Rep { rRound :: Integer, rTop :: Integer }

memoise :: Integer -> Integer -> State -> Integer -> (State, Maybe Repeat)
memoise repeatThreshold totRounds st@State{sMemo, sTower, sMemoTop, sTop} idx =
    if (totRounds - idx) < repeatThreshold then updateMemo else
        case sMemo Mp.!? key of
            Nothing -> updateMemo
            Just (idx0, top) -> (st, Just Rep{ rRound = idx0, rTop = top })
    where 
        key = show sTower
        updateMemo = (st{sMemo = Mp.insert key (idx, sMemoTop + fromIntegral sTop) sMemo}, Nothing)

type Memoise = State -> Integer -> (State, Maybe Repeat)

keepFalling :: Integer -> State -> Integer -> State
keepFalling repeatThreshold state totRounds = falling totRounds state shapes (memoise repeatThreshold totRounds)
    where 
        falling :: Integer -> State -> [Shape] -> Memoise -> State
        falling 0 st _ _ = st
        falling idx st (shape:rest) toMemo = 
            let State{sTower, sHeight, sTop} = st
                -- rockTop = getRockTop sTower 
                shapeBase = trace (show idx ++ "@" ++ (show $ sMemoTop st )++ ", sTop" ++ show sTop) (sTop + minSpace)
                newHeight = shapeBase + shapeHeight shape
                yOffset = newHeight - sHeight
                grownTower = sTower M.<-> newTower yOffset 
                positionedShape = positionShape (shapeBase + 1) shape
                nextState = fallBy1 st{sTower = grownTower, sHeight = newHeight } positionedShape
                memoised = toMemo nextState idx
            in case memoised of
                (st1, Nothing) -> falling (idx - 1) st1 rest toMemo
                (st1, Just rep) -> 
                    let (roundsLeft, fastState) = fastTrack st1 totRounds idx rep 
                    in falling roundsLeft fastState rest (\st2 _ -> (st2, Nothing))
        shapes :: [Shape]
        shapes = concat $ repeat [horiline, cross, club, vertiline, brick]

fastTrack :: State -> Integer -> Integer -> Repeat -> (Integer, State)
fastTrack st@State{sTop, sMemoTop} tot roundIdx Rep{rRound, rTop} =
    let currentTop = sMemoTop + fromIntegral sTop 
        diffTop = currentTop - rTop
        diffRounds = rRound - roundIdx
        repCnt = (roundIdx - 1) `div` diffRounds
        roundsLeft = roundIdx - repCnt * diffRounds - 1   -- excluding the current round
        stats = show [ show roundIdx, show tot, show rRound, show rTop, show currentTop, show diffTop, show repCnt, show roundsLeft, show $ sMemoTop + diffTop * repCnt ] 
    in (roundsLeft, st{sMemoTop = sMemoTop + diffTop * repCnt})

getScore jets rnds = 
    let State{sMemoTop, sTop} = keepFalling (fromIntegral $ L.length jets) (initialState (concat $ repeat jets)) rnds
    in sMemoTop + fromIntegral sTop



-- optimised :: Integer -> Jet -> Integer
-- optimised tot jets = 
--     let repeatNo = fromIntegral $ L.length jets * 5
--         times = tot `div` repeatNo
--         toGo = tot - times * repeatNo
--         repeatedJets = concat $ repeat jets
--         st1@State{sMemoTop=smt1, sTop=sTop1} = keepFalling (initialState repeatedJets) repeatNo
--         rockHeight = smt1 + fromIntegral sTop1 - 1
--         repeatedHeight = rockHeight * times - fromIntegral sTop1 -- count out sTop once as it kept in st2 below 
--         State{sMemoTop, sTop} = keepFalling st1{sMemoTop = repeatedHeight} toGo
--     in sMemoTop + fromIntegral sTop


-- ghci> (upset $ M.setElem '#' (4, 7)  $  newTower 4) M.! (1, 7) == '#'
-- True
-- ghci> (upset $ M.setElem '#' (1, 7)  $  newTower 1) M.! (1, 7) == '#'
-- True
-- ghci> (upset $ M.setElem '#' (5, 7)  $  newTower 5) M.! (1, 7) == '#'
-- True
-- ghci> (upset $ M.setElem '#' (3, 7)  $  newTower 5) M.! (3, 7) == '#'
-- True
-- ghci> (upset $ M.setElem '#' (2, 7)  $  newTower 5) M.! (4, 7) == '#'
-- True
upset :: M.Matrix a -> M.Matrix a
upset mx = foldl swap mx [1..totalRows `div` 2]
    where 
        totalRows = M.nrows mx
        swap mx1 n = M.switchRows n (totalRows - n + 1) mx1


jetPatterns :: Jet
jetPatterns = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

jetsTrue :: Jet
jetsTrue = ">><>><<><>><>>><<><<<>><<<><>>>><<<>>><<>><<<<>><<<>>>><<<>>><<<<>><<<>>><<<<>><<>>>><>>>><<><<>>>><<>><>>>><<>>>><<<<>>><>><>><<<<><>>><<>>><<><<<><<<<>><><<>>><<<>><<<<>>>><><<<>>>><>>>><<<><<>>><<>><<<>>>><>><>>>><><<<<>><><<<<>><<<<>><><>><<>>>><>>><<<>><<>><<>>><<<>><<<><>>><<<<>>>><<<<>>>><<<>>><>>><<<<>>>><<<<><<>>>><<<>>><<<><><>>>><<<<>>><<>>><<<>>><>>><<<>>>><><<<<>>>><>><<<>><<><<<<>><<<>><>>><<<<>>>><<<<>>>><<<>>><<<>>>><>>><<<><<<>>><<>>>><<<>>><<<>>>><<<<>><<<<>>><<<<><>><<<<>>><<<<>>>><<>>><>><<<<><<<<>>>><<<>>>><<><<<<><<<>><<<<>><>>><<><<<>>><<>><<<<>><<<>>><>>>><<>>><<<>>>><<>>>><<<><<<<>>>><<>>>><>><<<<>><<<>>>><<<<><>><><<>>><<<<>>><<<><><<<<><<<<>><<><>>><>>>><<<>>><<<>><>>>><<<<>><<<>><<>>><<<<>><<<>><<<><>><<>>><>>><<<><<<<>>><<<<>>>><<<><<<><<><<<><<<><<<<><<<>><><>>><<<><<>>><<<>><>><<<><>>>><<<<>>>><<<>>>><<>><><<<<>>><<<<>>><<<<>>>><><<>>>><<<<>>><<<>><<>>>><<<><>><<>>><<<>>>><<<>>>><<<<>><>><<<><<<>>><<>>><<>>>><<<<>><<<>>><>>><<<<>><<<<>>>><<<>>>><<<<><<<<>>><<<><<<<>>><>><<<<>>>><<><>><<<>>><<<>><<>>>><<>>><<>>><<<<>>>><>>>><<<>><>>>><<<<>>>><><<<<>>>><<<<>>>><<<<><<>><<<><<><<<>><<>><<<<><<<>>>><<<<>>>><<><>>>><<<>><<>><<<>>><<<<>>><<<>><<<<><<<<><>>><<<<>><<>>>><<<>>>><<<<>>>><<>>>><<<>>>><>><>>><<>>><<<<>>>><<<>>>><<>>>><<>>>><<<<>>>><>><>><<>><<<<><><<<>><<<<><<<>><<><<>><<<<><<>>>><><<<>>><<<><<<<>>>><>>>><>>>><<<><<<><>>>><><<<>>><><<>>><>>><<<>><<<<><<>>>><<>>><<<<><>><<<<>>><<>><<<<>><<<<>>><<>>>><<>><<>>>><<<>>>><<>>><>>><<<>>><<<<>>><>>><<<<>><<>><<<<>><>><<<<><>><>>>><<<>><<>>><>>><<<>>>><<<<>><<<<>><<>>><<>>>><<<<>>>><<<>><<>><>>>><<<><>><<<>>>><<<>>>><<<>><<><><<<>><<<<><>><>>>><<>><<<><<<<><<<<>>><<<><<>>><<>><<<<>>><<<>><<>>><>>>><<>><<<<><<>><<<<>>><<<>>><<<>><<>>><<<<><<>><<><<<<>><>><>><<>>><>>><<<<><<<>>><<<>>>><<<<>>>><<<<>>><<><<<>>>><<<<>>>><><>>><<<<>>><<<<>>>><<<>>>><<<><<>>>><<<<>>>><>>>><<<><<>>><<<>><>>><<<<><<>>>><<>>>><<<><<<<>>><<<<><<<><<<>>><<<><<<<>><>><<<><<>>><<<>><<<<><<<<><<<<><>>>><<>><>><<<>>>><<<>>><>>>><<<<>><<<><>>><<<<>>><>>>><>>>><<<<>>><<<<>>><<<>><<<<>><<>>><<<<>><<<>>>><>>><<<<>>><><<<<>>><>><<<>>><<<>>>><<<<><<<><<>>>><<<>>><>>>><>>><<>><<<><<>>>><<<<>><<<<><><<>>>><<><<>>>><<>>>><<<<>><<<<><><<<><>><<<<>>><<<<>><<<<>><>><<<<>>>><<<<><>><<<>>>><>>><<<>><<><<<>><<>>><<<><>>><<<<>>><><<<<><<<<>>><<<>>>><<<>>><>>><<<<><<<>>><>><<>><><<<>><<>>><<<<>><<<>>>><<>><>><>><<<<>>><<<><<>>>><<<<>><<>>>><<<>>><<<>><<><>>><<<<><<<>><<<<>>>><<>>><<<>>><>>><<<>>>><<>><<>>><<>><>>><<<>>><<>><<>>><<<>>><<<>><>><<<>>><>><<<>>>><<>><<>><<>>><<>>><<<<>>><<<><<>>>><<>>><<>>>><<<<><<<<>>><<<<>><>>>><<<>>><<<<>><<<<>>>><>>>><>><<<><>>><<>>><<<<>>>><<<<>><<><<><>>><<<<><<<>>><>>><><<>>><<><<><>><<<><>>>><>>>><<>>><<>><<<<>>>><<<>><<<<>>><<<<><<>>><<<<>>>><<<<>>>><><>>><<>><<>>><>>>><<<>>><<<<><>><>>><<<<>>><<<>><<>>>><>>>><<<><<<<><<<<>><<>><<<<>>>><>>>><>>>><<<><<>>><>><>>><<><>><<>>>><<>>><<<>>>><<<<>>>><>><><<>>><<<><<<<>>><>>><>>><<<>>><<<<>><<>><<<<>>><><<<>>>><<<<>><>>><<<<>>><>><>><>>>><<>>><<<<>>>><<<<>>>><<<<>>><<<<>>><<<>><<<<>>><<><<<>>><<<<>>><<<<>>><<>>>><<<>><>><>>><<<<>>><><<<<><<<><<<<>><>>>><<<<>><<<<>>><<<<>>>><<>>><<<<>><>><<>><<<>>>><<<>>><<<>>><<<<>>>><<>>>><<>>>><<<><<<<><<><<<<>><><>><>>><>>>><<<><>>>><<><>><><<<>><<<<>>>><><<><<<>>><<<<><<>>><<<>><<<<><>><>>>><>>><<<><<>><><<<>><<>>><<><<<<>>><<>><<<><<<><><<<<>>><<<<><<<><<<<>>><>>><>>>><<<>>><>>><><<>>>><<<>>><<<>>><>>>><<<>>>><>>><><<>>>><<<<>><<<>><>>><<>>>><<>>>><><><<<<>>>><<<><<>>>><<>><<<><<><>>><>>><<>>>><<><<<>><<><<<<><<<<>><<<<>>><<<>>>><<<<>>>><<<>>>><>>><>>><<>>>><<<<>>><>>><<>>><<<>><<>>><<<><<<<>>>><<<<>>>><<><<<><<><<><>><<<>>>><<<>>><<<<>><<<>><<>><>>>><<<<>>>><<<<>>>><<<>>><>>><><<>>><<<<>><<>>><<<<><>>><>>>><>><>><<<<>>>><>>><<<><<>>><>>><<><>><<>>><<<>><<<<>>>><>>><>>>><>>>><<<<><<>><<<>>><>>><>>>><><><<<<>><>><>>><<<>>><<<<>><<<><><<>>>><>>>><>>>><<<<>>>><<>>>><<<<>>>><>><<<>>><<>>>><<<><<<<>><>>>><<<>>><>>>><<<<><<>><>><>>>><<<<>>><<<<>>>><><>><<<>><><<<><<>><<<<>>>><<<>>>><<<>><><<<<><<<<>>>><<>>><<<<>><<<>><><<<<><<><<<>>><>><<<<>><>>>><<<<><<>>><<<>>>><<<<>>>><<<<><>><<<>><>>>><<<<>>>><<>>><<<<>>>><>><>>>><>>><<<>>>><<<>>><>>>><<<<>>><<<>>><<>><<<<>><<><>><<<<>>>><<>>>><<>><<>>><<<<>><<>>><<<<>>><><>>><<<>><<<>>><<>>><>>>><>>><<>>>><<<<><>>>><<<<><<<>>>><>>><<<>>><<<<>>>><<<<>>><>>><<<><>>>><<<>>><>>><<><<>>>><<><<<<>><<<><<<<>><>><<<<>><>>>><<>>><<<>><<<>>>><>>>><<<<>>>><><<<<>>>><<<<>><<<>>>><<<>><<<<>>>><<>>><>>><<<<>>><>>><><<<<>>>><<<>>><<<<>>>><>>>><<<<>><<<<>><<<>>><<<<>>><<<<>>>><<>><>>>><<<>><<<><>>>><<<<>><<>><<<<>><>>><>>>><<<><<<<>><>><<<<>><<<><<<><><<>>><>>><<<<>><><<<><<<<>>>><>><<<<>>>><<<<>>><<>><<>>>><<<<><<<><<<<>><<<<>>><><<><<<><>>><<<>>>><<<>>><<<<>><<>><<<<>>>><>><><<<><><>><<<>><<<<>>>><<<>>>><<<<>>>><<>>>><>><<<>>><<<>>><<>>><<<<>>><><<>>>><<<>>><>>>><<<>><<<<>><<<>>><<><<><<>><<<><<<>><<<>>><<>><<<<>><<><<<<><<<<>>>><>>>><>><<>>><<<<>>><<>><><>><<<>><<>>><<<<>><><>>>><<<>>>><<><<<><<><<<<>>><<<>><<<>><<<>>>><<>>>><<<<>><<<<>><<<<><<><<>>><<<>>><<>>><<<>>><<<<><<<<>>><<<><<<>>><<<>><<<>><<<<>><<<>><>>>><>><<<<>>>><<<<>>><><<<>>><<>>><><>><<>>><<<><<<>>><<<>><>>><<<>>>><<<>><<<<><<<>>><<<<><<<>>>><><><<<>><><<<><<<<>>><<><<<<><<<>>><<<<><<<<>>><<>>><<>>>><<>>>><<>>><><<<><<<<>><<<<>><<<>><<<<>>><<>>><<<<>><<<<>>>><<>>><<<<>>>><<><<>><<<>><<<<>><<<<>>>><<>>>><<<>>>><<<<>>><>><>>><<<<>>><<>>><<<>>>><<<<>>><<<<>><<>>>><<>>><<<>>>><<<>><<<><<<>>>><<<<>>>><<<>><>><<>>><<<<>><>>>><<><<<>>>><<>>>><<<<>>><<<>><<<<>>><<<<>>>><<<<><<>>>><>><>>><<<>>>><>>>><>>>><<<><<>><<><<>><<<<><<>>><><<>>>><<<<><<<<><<<>><<<>><>>><<><<<><>>>><>><<<>>><<>>><<<<>>>><<<>><<<<>>><<>><<<>><<<<>><>>>><>><<<>>><<<<>>>><<>><<<><<>><<<>><<<<><<<>><<>><<<>>>><<<<>>><<<>><<<><<>>>><<>>>><<<<>><>>>><<<>>><<<><>><<<>>>><>>>><<<<>>>><<<>><>>>><<<<>><<<<>><<<<>><<>><><<<>><<<>>><<><<>><<<<>>>><<>>><><>><>><<>>>><<<<><<>>><<<<>>>><<<>>><<>><<><>><<<<>>><<>><<>>><>>>><<<<>><>><<<><<>>>><><<<><><<>>>><><<>>><<<>>><>>>><<><<>><><><<<<>><<<<><<>>>><>>><<>>><<>>><<<>>><><<>>>><<>>><<<>>>><<<>>><>><<<<><>>>><<<>>><<<>>><<><<>><<<<>>><<<<>><<>>>><>>>><<>><<<<><<<<>><<<<><<<>>>><<><<<<>>>><<<<>>>><>>>><<>><<><<<>>>><<<><<>><<<<>>><<<><>>><>><<<>><><>><<<<>><<<<>>>><<<<><<>>>><>>>><<>>><<<<>><>>><<<>>><<><<>>><>><<>>><><<>>>><>><<<>>>><<>><<>>><<<>>>><><<><<><<>><<><<<<>>>><<<>>><>><<<<>>>><<<<>>>><>>><<<>><<>>>><<>><>>>><>>><<>>>><<<><>><<<>><<<<>><<>><<<>>><<<>>><<<<>><>>>><<<><<<><<>><<<<><<<>><><<><<<<><<<>>>><<<><>><<<><<<>><>>>><><<<<>>><<>>>><<>>><>>>><<>>><<<>>><>><<<<>>><><<><<<<>><>>>><<>>>><>>>><<><>><<<<>><<<<>>><><<<>><<><<<><<<>>><<><<<><<><><>>><<<<>><<><>>>><<<>>>><<<<>><<<<>>><>>>><<>>><<<<>><>>>><<<<>><>>><<><>>><<<><>>>><<<<>>><>><<<<>>>><<<>>>><<>>><>>><><<><<<<>><<<><>>>><>>>><<<>>><>>>><<<><<<>><><>><<<><>>><>><>><>>><<<<>>><<<<>><<<<>>>><<<>><<<>>>><<>>>><<>>>><><<<>>>><<>><<<>><<>>>><<<>>><<>>><<<><<<>><>>><>><<<<>><<<>><>>><<<<>><>>>><>><<><<<<>><>><<<<><<>>><<<><>>><<<><<><<<>><<<<>>><<<>>>><<<<>><<>>><>><><<>>><>><<<>><<>>><<>>>><<>>><<>>>><>>><>>><<>><<>>>><<<><<<<>>><<<>>><>>>><<<>>><<>><><<>>>><<<<>>><<><<>>>><>><>>><<<<><<<<>><<>><<<<>><>>>><>><<<>>>><<>><<<>>><<<>>>><<<>><<<><<<<>><<>>>><<<>>>><<<><>>><<>><<<<>>>><<>><<>>><><<<<><<<<><<>>>><<<>>>><<<<><<>>>><<<<>><>><<<>><<>><<<<>>><<>>><<<>><>>><<><<<>>><>>><><<<><<<<>>>><<<<>>>><<<<>>><<>>><<<>>>><<<>><<<<>>><<<<>>>><<<>>><<<>>><<<<><>>><>><<>>>><>>>><<<<>><<>><>>>><<>>>><<<<>>><<<><<<<>>>><<<>>>><>>>><<>>>><<<<>>>><<><<>>>><<<><<<>><<<>><<>>>><<<>>>><<<>><<<<>>>><>>>><<<><>>>><<>>><<<<>><><<<><>>><<>>><<<<>>><<<<>>><>>>><<>>>><<>><<<<><<<>>><<>>>><<<><<<>>><<>><<<>><<>><<><>><<<><<><<<>>>><><<><>>>><<<>>><><<<>>>><<<>><<<<><<<><>>><>>>><<<><<>><>>><<>><<<<>>><<<><<<>>>><<><<>>><<>><>>>><<<<>>><>>><<><<<>>>><><<<>>><><>>><>>><<<<><<<<><<<<>>>><>>>><<<<>><<<><<>>>><>>>><>>><><<<>>><<<<>><<>><<>>>><<>><><>><>><<<<>><<<>>>><<<><<<<><<<<><<<>>>><<><<<<>>>><<<>><>>>><>><<<>>><<<>>><<<<><<<<><<<>><<<<>><>><<<>><<<<>>><<<<><><<<><<>>>><<><<<<>>>><<<><>>>><<><<<<><>>><<<>>><<<<>>><<>><>><<<<>>><<>><<<<>><<<>><>><<<<><>>>><<<>><>>>><<<<><<<>><<>><<>><<>>><<<><<>>><<<>>><<<<>>>><><>>><<<<>>><>>><<<>><>><<<<>>><<>>><<><<><><<<><<>><<<>>>><<>>><>>>><>>><<>>>><<<<>><<>>><<<><<<>><<<<>><<><>><><<><>><<>>>><<<>>>><<<<>><<<<>><<<>><<<<><<>>><<<<>>>><<<>><<<<>>>><>>>><<<<><>><>><<<<>>><<>>>><<<<>><>><>>>><<<<><<>>>><<>><<>>>><<<<>>><<<<><>><>>><<<<>>>><<<>>><<><<><<<<>><<<>>>><>><<>><><<<<>>><<<>>>><<<<>>>><>>><<<<>>><<>><>>>><<>>><>>>><<<<><<>>>><<>>><<>><<<<>>>><<><<<><><<<>>><>>>><<<>>>><<<>>>><<><<>>>><<<><<>>><<><<><<>>>><<>>><<>>><<<<>><<<>>>><<<>>>><<<><<<>>><<<><<<>><<<<><<<><<>>>><>>>><<<<>>>><<>>>><<<<><<<>>>><<<>>>><><>>><<>>>><<<>>><<<<>>><>>>><<>><>><><<<>><>><<><<<<>>>><<<>><<<<>><><>>>><><<<<><><<<<>>><<<>>><<<<>>><<<<>>><>>><<<>>><<<<>>><<<>>><<>>><<>>>><<<>>><<<<>>>><<<<><><<<>>><<<><<<<>><<<<>>><<<>>>><<<<><<<>>><<>>><<<<><>>><<><<<<>>>><<<<>>><>>><<<<>><<<<><><>><>><<<<>>><<<<>><<<<>>><>>>><<<<>>><<<>>><<>>><<><<>>>><>><>>>><>><>><>>><<><<><><<<>>><<><<>>>><<<>>><<<><>>><<<>>>><<<<>>><>>>><>>><<<><<><>>>><<><<<>><<>><<<>><<<<>><<>>>><<<<><<<><>>>><<><<>><<<<>><<>>><<>><<<<>>>><<<>>>><<<>>><<<>>>><<>><<<<><<<><<><<<>><<<<>>><<<>><>>>><<<<><<<<>>><>>>><<<>><<>>>><>>><<<>><<<<>>>><<>><<><>>><<<<><><<<>><<<>><<<<>>>><<<><>>>><<><<<><<<<>>><><><<<>><<<>><<<<>>><<<>>><<<<><<<>>>><>><<><<>>>><<<>>>><<>>>><<<<>>><<>>>><<<>><<<><<<>>><<>>><<<<><<<>>>><<<>>><<<<>>><>><<<>>><<>>><<<>>><<>><<<<>><>>><<<>><>><>>><<<><>>>><<<<>>><<<<><>>><>>><<<>>><<<>>><<>>><<<>><<<>><<<<><><><><<<>>>><<<<>>><<><>>><>>>><<>>><<<<>>><<<><>><<<>>><>>>><>>><<>><<<<>>><<<><><<<<>>><<<>>>><<>>><<<<><><<>>><<<<>><<>>>><<<>>><<<<>>><<>>><<<>>><<<<>>><<<<>>><<><<><<<>>><<<>>>><><<>>>><<<>><<>>><<<>>>><<<><<>><>>>><<<>><<>><<>>>><<<><><<<>><<>><<><<<><>>><<><<<<>>>><>>><>>><<><<<>><<><<<>>>><<<>>><><<<<>><<<<><<<<>>><<<<>><<>><<<<><<<<>>><<<<>>>><<>>><<<>>><<>>><<<>>><<<>><<>>><>>><<<>><<>><><>><>>><<>><<><>>>><>>>><<>>>><<>>><<<>>><<<<>>>><<<<>>>><<<<>>><<><>>>><>><<>>>><<<<>>>><<>>><<<<>><<<>>><<<<>><>>>><<<<>><<<><<><<<<>"