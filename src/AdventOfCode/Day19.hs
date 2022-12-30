{-# Language NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module AdventOfCode.Day19 where

import Data.Function ((&), on)
import Control.Lens
import qualified Data.List.Ordered as PQ
import qualified Data.List as L
import Control.Parallel.Strategies
import Debug.Trace

-- trace x y = y

-- newtype Ore = Ore { unOre :: Int } deriving (Eq, Show)
-- newtype Clay = Clay { unClay :: Int } deriving (Eq, Show)
-- newtype Obsidian = Obsidian { unOb :: Int } deriving (Eq, Show)
-- newtype Geode = Geode { unGeode:: Int } deriving (Eq, Show)

data Blueprint = BP {
    _oreRobotCost :: Int,
    _clayRobotCost :: Int,
    _obsidianRobotCost :: (Int, Int),
    _geodeRobotCost :: (Int, Int) 
    } deriving (Eq, Show)


makeLenses ''Blueprint

data State = State {
    _oreRobots :: Int,
    _oreCount :: Int,
    _clayRobots :: Int,
    _clayCount :: Int,
    _obsidianRobots :: Int,
    _obsidianCount :: Int,
    _geodeRobots :: Int,
    _geodeCount :: Int,
    _sRound :: Int,
    _blueprint :: Blueprint,
    _preState :: Maybe State
    -- _projectedGeode :: Int
    } deriving (Eq, Show)

makeLenses ''State

-- ghci> projectObsidian [6] 7 2
-- 1
-- ghci> projectObsidian [6] 7 7
-- 1
-- ghci> projectObsidian [6] 7 8
-- 2
-- ghci> projectObsidian [3,4] 7 3
-- 1
-- ghci> projectObsidian [3,4] 7 4
-- 2
-- projectResource :: Int -> Int -> Int -> Int
-- projectResource _ _ 0 = 0
-- projectResource currentSum cost rounds =
--     let projected = (+ currentSum) <$> [0..rounds]
--     in breakOnCost (0, projected)
--     where 
--         breakOnCost (cnt, []) = cnt
--         breakOnCost (cnt, rest) =
--             let (taken, rest1) = break (\s -> s >= cost) rest
--             in if null rest1 
--                 then cnt
--                 else 
--                     let (good:rest2) = rest1 
--                         generated = good `div` cost
--                         consumed = generated * cost
--                     in breakOnCost (cnt + generated, (\x -> x - consumed) <$> rest2)

-- projection st = 
--     let n = st ^. sRound
--         bp = st ^. blueprint
--         maxGeode = n * (st ^. geodeRobots) + st ^. geodeCount
--         -- projected = projectResource (st ^. obsidianCount) (bp ^. geodeRobotCost._2) n -- at least one minute is required
--     -- in if n < 5 then maxGeode else maxGeode + projected
--     in maxGeode

instance Ord State where
    compare st2 st1 = -- mind it's reversed so higher is first
        (st1 ^. geodeCount) `compare` (st2 ^. geodeCount) <>
        (st1 ^. geodeRobots) `compare` (st2 ^. geodeRobots) <>
        (st1 ^. obsidianCount) `compare` (st2 ^. obsidianCount) <> 
        (st1 ^. obsidianRobots) `compare` (st2 ^. obsidianRobots) <> 
        (st1 ^. clayCount) `compare` (st2 ^. clayCount) <> 
        (st1 ^. clayRobots) `compare` (st2 ^. clayRobots)

sumHeads xs = sum xs

initialState bp = State {
    _oreCount = 0,
    _oreRobots = 1,
    _clayCount = 0,
    _clayRobots = 0,
    _obsidianRobots = 0,
    _obsidianCount = 0,
    _geodeRobots = 0,
    _geodeCount = 0,
    _sRound = maxMinutes,
    _blueprint = bp,
    _preState = Nothing
    -- _projectedGeode = 0
    }

collect stOld stNew =
    stNew &
    oreCount %~ (+ stOld ^. oreRobots) &
    clayCount %~ (+ stOld ^. clayRobots) &
    obsidianCount %~ (+ stOld ^. obsidianRobots) &
    geodeCount %~ (+ stOld ^. geodeRobots) -- &
    -- (\s -> projectedGeode .~ (projection s) $ s)


next :: State -> [State]
next st =
    st &
    sRound %~ (\x -> x - 1) &
    makeGeodeRobot (st^.blueprint.geodeRobotCost) &
    concatMap (makeObsidianRobot $ st^.blueprint.obsidianRobotCost) &
    concatMap (makeClayRobot $ st^.blueprint.clayRobotCost) &
    concatMap (makeOreRobot $ st^.blueprint.oreRobotCost) &
    fmap (collect st) &
    mapped.preState .~ Just st &
    L.sort . L.nub

maxMinutes = 24

-- ghci> fmap (fmap $ Prelude.take 2) $ resetRobots 1 [[2..]]
-- Just [[1,2]]
-- ghci> fmap (fmap $ Prelude.take 2) $ resetRobots 2 [[2..]]
-- Just [[0,1]]
-- ghci> fmap (fmap $ Prelude.take 2) $ resetRobots 3 [[2..]]
-- Nothing
-- resetRobots :: 
--     Int         -- number to take
--     -> Int  -- robots
--     -> Maybe [Int]  -- robots after reset
-- resetRobots tot robots = 
--     let (toTake, reset) = foldl reset1 (tot, []) robots
--     in if toTake > 0  -- not enough to take
--         then Nothing
--         else Just reset
--     where 
--         reset1 (0, reset) robot = (0, robot:reset)
--         reset1 (toTake, reset) robot = 
--             let taken = min toTake robot  -- in the case more is available
--             in (toTake - taken, robot - taken:reset)

takeResource :: Int -> Lens' State Int -> State -> Maybe State
takeResource needed lCnt st = 
    let available = st ^. lCnt
    in if available >= needed
        then Just (lCnt .~ (available - needed) $ st)
        else Nothing

addRobot :: Lens' State Int -> State -> Maybe State
addRobot lRobot st = Just $ lRobot %~ (+ 1) $ st -- starts with -1 as robot is built in a minute

makeGeodeRobot :: (Int, Int) -> State -> [State]
makeGeodeRobot (ore, obsidian) st = 
    case takeResource ore oreCount st >>= takeResource obsidian obsidianCount >>= addRobot geodeRobots of
        Nothing -> [st]
        Just st1 -> [st1]   -- always build geode

-- makeObsidianRobot (3, 14) State{_oreRobots = [[8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]], _clayRobots = [], _obsidianRobots = [], _geodeRobots = [], _sRound = 17, _blueprint = BP {_oreRobotCost = 4, _clayRobotCost = 2, _obsidianRobotCost = (3,14), _geodeRobotCost = (2,7)}}
makeObsidianRobot :: (Int, Int) -> State -> [State]
makeObsidianRobot (ore, clay) st = 
    case takeResource ore oreCount st >>= takeResource clay clayCount >>= addRobot obsidianRobots of
        Nothing -> [st]
        Just st1 -> [st1]   -- always build obsidian

makeClayRobot :: Int -> State -> [State]
makeClayRobot ore st = 
    if not $ worthyNewRobot st then [st] else
        case takeResource ore oreCount st >>= addRobot clayRobots of
            Nothing -> [st]
            Just st1 -> [st1, st]

makeOreRobot :: Int -> State -> [State]
makeOreRobot ore st = 
    if not $ worthyNewRobot st then [st] else
        maybe [st] (:[st]) $ takeResource ore oreCount st >>= addRobot oreRobots

worthyNewRobot st = 
    let inOrder = (st ^. sRound - 1) * (st ^. obsidianRobots + 1) + st ^. obsidianCount >= st ^. blueprint.geodeRobotCost._2   -- no new obsidian coming in
    in inOrder

blueprints :: [Blueprint]
blueprints = [
    BP { _oreRobotCost = 4, _clayRobotCost = 2, _obsidianRobotCost = (3, 14), _geodeRobotCost = (2, 7)},
    BP { _oreRobotCost = 2, _clayRobotCost = 3, _obsidianRobotCost = (3, 8), _geodeRobotCost = (3, 12)}
    ]

bpTest = blueprints !! 0

runBlueprint :: State -> [State] -> State
runBlueprint _best states = 
    let poolBest = maybe _best id $ fst <$> L.uncons states
        best = trace 
                (show [ _best ^. geodeCount, _best ^. obsidianCount ] ++ "Round " ++ (show (_best ^. sRound)) ++ " Pool size " ++ (show $ L.length states)) 
                $ min poolBest _best
        -- cutoff = best ^. projectedGeode 
        -- filteredByBest = PQ.filter (\st -> st ^. projectedGeode >= cutoff) nexts
    in
        let pool = L.filter (worthy best) states
            (onpar, subpar) = L.splitAt (L.length pool `div` 2) pool
            [xOnpar, xSubpar] = PQ.unionAll . PQ.sort . fmap next <$> [ onpar, subpar ]
            combined = PQ.union xOnpar xSubpar
        in if L.null states || (L.head states) ^. sRound == 0 
            then best
            else 
                if L.null xOnpar || L.null xSubpar
                    then 
                        if L.null combined then best else
                            runBlueprint (L.head combined) combined
                    else
                        let [onparBest, subparBest] = L.head <$> [ xOnpar, xSubpar ]
                            combinedBest = min onparBest subparBest
                            (onpar1, subpar1) = L.partition (\st -> st <= combinedBest) combined
                            topBest = runBlueprint combinedBest onpar1
                            bottomBest = runBlueprint topBest subpar1
                        in min topBest bottomBest

worthy best candidate = 
    let projectedCandidate = candidate ^. geodeCount + (candidate ^. geodeRobots + 1) * candidate ^. sRound
        projectedBest = best ^. geodeCount + (best ^. geodeRobots) * best ^. sRound
        shouldFilter = best ^. geodeCount > 0
    in 
        if shouldFilter 
            then projectedCandidate >= projectedBest || candidate ^. geodeRobots >= best ^. geodeRobots -- not enough robots
            else True
        -- byGeoCnt sts = if best ^. geodeRobots > 0 then PQ.filter (\st -> st ^. geodeRobots > 0) sts else sts
        -- byObsCnt sts = if best ^. obsidianRobots > 0 then PQ.filter (\st -> st ^. obsidianRobots > 0 || st ^. clayRobots > 0 ) sts else sts
        

runGroup :: [Blueprint] -> Int
runGroup bps = 
    let scores = [ (idx * totalGeode bp) | (idx, bp) <- indexed ]
    in sum (scores `using` parList rdeepseq)
    where 
        indexed = zip [1..] bps
        totalGeode bp = 
            let initSt = initialState bp
                best = runBlueprint initSt (L.singleton $ initSt)
            in best ^. geodeCount

testBlueprints :: [Blueprint]
testBlueprints = [
    BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(4, 9), _geodeRobotCost =  (3, 9) },
    BP { _oreRobotCost = 4, _clayRobotCost = 3, _obsidianRobotCost =(4, 20), _geodeRobotCost =  (4, 8) },
    BP { _oreRobotCost = 2, _clayRobotCost = 3, _obsidianRobotCost =(2, 16), _geodeRobotCost =  (2, 9) },
    BP { _oreRobotCost = 3, _clayRobotCost = 4, _obsidianRobotCost =(4, 20), _geodeRobotCost =  (4, 16) },
    BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(4, 16), _geodeRobotCost =  (2, 15) },
    BP { _oreRobotCost = 2, _clayRobotCost = 2, _obsidianRobotCost =(2, 20), _geodeRobotCost =  (2, 14) },
    BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(3, 7), _geodeRobotCost =  (3, 20) },
    BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(3, 14), _geodeRobotCost =  (4, 15) },
    BP { _oreRobotCost = 4, _clayRobotCost = 3, _obsidianRobotCost =(3, 7), _geodeRobotCost =  (2, 7) },
    BP { _oreRobotCost = 3, _clayRobotCost = 3, _obsidianRobotCost =(2, 11), _geodeRobotCost =  (2, 19) },
    BP { _oreRobotCost = 3, _clayRobotCost = 3, _obsidianRobotCost =(3, 20), _geodeRobotCost =  (2, 12) },
    BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(4, 20), _geodeRobotCost =  (2, 8) },
    BP { _oreRobotCost = 2, _clayRobotCost = 4, _obsidianRobotCost =(3, 14), _geodeRobotCost =  (4, 9) },
    BP { _oreRobotCost = 3, _clayRobotCost = 4, _obsidianRobotCost =(4, 18), _geodeRobotCost =  (3, 8) },
    BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(2, 9), _geodeRobotCost =  (3, 15) },
    BP { _oreRobotCost = 2, _clayRobotCost = 3, _obsidianRobotCost =(3, 11), _geodeRobotCost =  (2, 16) },
    BP { _oreRobotCost = 2, _clayRobotCost = 3, _obsidianRobotCost =(3, 13), _geodeRobotCost =  (3, 15) },
    BP { _oreRobotCost = 3, _clayRobotCost = 3, _obsidianRobotCost =(3, 16), _geodeRobotCost =  (3, 20) },
    BP { _oreRobotCost = 2, _clayRobotCost = 4, _obsidianRobotCost =(3, 19), _geodeRobotCost =  (4, 8) },
    BP { _oreRobotCost = 4, _clayRobotCost = 3, _obsidianRobotCost =(4, 16), _geodeRobotCost =  (2, 15) },
    BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(4, 7), _geodeRobotCost =  (2, 19) },
    BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(2, 14), _geodeRobotCost =  (3, 17) },
    BP { _oreRobotCost = 4, _clayRobotCost = 3, _obsidianRobotCost =(4, 8), _geodeRobotCost =  (2, 8) },
    BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(4, 7), _geodeRobotCost =  (4, 17) },
    BP { _oreRobotCost = 3, _clayRobotCost = 3, _obsidianRobotCost =(3, 16), _geodeRobotCost =  (3, 9) },
    BP { _oreRobotCost = 4, _clayRobotCost = 3, _obsidianRobotCost =(4, 15), _geodeRobotCost =  (4, 9) },
    BP { _oreRobotCost = 3, _clayRobotCost = 4, _obsidianRobotCost =(2, 20), _geodeRobotCost =  (4, 7) },
    BP { _oreRobotCost = 3, _clayRobotCost = 3, _obsidianRobotCost =(3, 17), _geodeRobotCost =  (4, 8) },
    BP { _oreRobotCost = 3, _clayRobotCost = 4, _obsidianRobotCost =(3, 12), _geodeRobotCost =  (3, 17) },
    BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(4, 5), _geodeRobotCost =  (2, 10) }
    ]