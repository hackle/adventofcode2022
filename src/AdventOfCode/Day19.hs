{-# Language NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module AdventOfCode.Day19 where

import Data.Function ((&), on)
import Control.Lens
import qualified Data.List.Ordered as PQ
import qualified Data.List as L
import Control.Parallel.Strategies
import Debug.Trace
import qualified Data.Set as S

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

data ActionHint = BAU | SkipOreRobot | SkipClayRobot | SkipObsidianRobot | SkipGeoRobot deriving (Eq, Show)

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
    _preState :: Maybe State,
    _actionHint :: ActionHint
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
        (st1 ^. clayRobots) `compare` (st2 ^. clayRobots) <>
        (st1 ^. oreCount) `compare` (st2 ^. oreCount) <>
        (st1 ^. oreRobots) `compare` (st2 ^. oreRobots) <>
        (st2 ^. sRound) `compare` (st1 ^. sRound)   -- lower the minute the better

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
    _preState = Nothing,
    _actionHint = BAU
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
    makeGeodeRobot (st^.blueprint.geodeRobotCost) &
    concatMap (makeObsidianRobot $ st^.blueprint.obsidianRobotCost) &
    concatMap (makeClayRobot $ st^.blueprint.clayRobotCost) &
    concatMap (makeOreRobot $ st^.blueprint.oreRobotCost) &
    fmap (collect st) &
    -- mapped.preState .~ Just st &
    mapped.sRound %~ (\x -> x - 1) &
    PQ.nub . L.sort

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
    if tooLateToBuy then [st] else
        case takeResource ore oreCount st >>= takeResource obsidian obsidianCount >>= addRobot geodeRobots of
            Nothing -> [st]
            Just st1 -> [actionHint .~ BAU $ st1]   -- always build geode
    where
        tooLateToBuy = st ^. sRound <= 1
        -- +1 to make new robot

-- makeObsidianRobot (3, 14) State{_oreRobots = [[8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]], _clayRobots = [], _obsidianRobots = [], _geodeRobots = [], _sRound = 17, _blueprint = BP {_oreRobotCost = 4, _clayRobotCost = 2, _obsidianRobotCost = (3,14), _geodeRobotCost = (2,7)}}
makeObsidianRobot :: (Int, Int) -> State -> [State]
makeObsidianRobot (ore, clay) st = 
    if st ^. actionHint == SkipObsidianRobot || tooLateToBuy || noExtra then [st] else
        case takeResource ore oreCount st >>= takeResource clay clayCount >>= addRobot obsidianRobots of
            Nothing -> [st]
            Just st1 -> [actionHint .~ BAU $ st1, actionHint .~ SkipObsidianRobot $ st]   -- always build obsidian
    where 
        noExtra = st ^. obsidianRobots == st ^. blueprint.geodeRobotCost._2 
        tooLateToBuy = st ^. sRound <= 3
        -- + 1 to make new robot (this minute)
        -- + 1 to produce new obsidian
        -- + 1 to make Geode robot

makeClayRobot :: Int -> State -> [State]
makeClayRobot ore st = 
    if st ^. actionHint == SkipClayRobot || tooLateToBuy || noExtra then [st] else
        case takeResource ore oreCount st >>= addRobot clayRobots of
            Nothing -> [st]
            Just st1 -> [actionHint .~ BAU $ st1, actionHint .~ SkipClayRobot $ st ]
    where
        noExtra = st ^. clayRobots == st ^. blueprint.obsidianRobotCost._2
        tooLateToBuy = st ^. sRound <= 5
        -- 1 minute to make clay robot (this minute)
        -- + 1 minute to produce a clay 
        -- + 1 minute to make an obsidian robot
        -- + 1 minute to produce obsidian
        -- + 1 minute to make a geode robot

makeOreRobot :: Int -> State -> [State]
makeOreRobot ore st = 
    if st ^. actionHint == SkipOreRobot || tooLateToBuy || noExtra then [st] else
        case takeResource ore oreCount st >>= addRobot oreRobots of
            Nothing -> [st]
            Just st1 -> [actionHint .~ BAU $ st1, actionHint .~ SkipOreRobot $ st]
    where
        noExtra = st ^. oreRobots == st ^. blueprint.oreRobotCost
        tooLateToBuy = st ^. sRound <= 7
        -- 1 minute to make ore robot (this minute)
        -- 1 minute to produce ore (this minute)
        -- 1 minute to make clay robot (this minute)
        -- + 1 minute to produce a clay 
        -- + 1 minute to make an obsidian robot
        -- + 1 minute to produce obsidian
        -- + 1 minute to make a geode robot

blueprints :: [Blueprint]
blueprints = [
    BP { _oreRobotCost = 4, _clayRobotCost = 2, _obsidianRobotCost = (3, 14), _geodeRobotCost = (2, 7)},
    BP { _oreRobotCost = 2, _clayRobotCost = 3, _obsidianRobotCost = (3, 8), _geodeRobotCost = (3, 12)}
    ]

bpTest = blueprints !! 0

runBlueprint :: State -> State
runBlueprint state = go Nothing [state] S.empty
    where
        go :: Maybe State -> [State] -> S.Set State -> State
        go Nothing [] _ = error "This shouldn't have happened!"
        go (Just best) [] _ = best
        go bestem togo@(candidate:rest) beenTo = 
            if candidate ^. sRound == 0 
                then 
                    let newBest = maybe candidate (min candidate) bestem
                        (left, beenTo1) = L.partition (\s -> not (s `S.member` beenTo || noHopeVs newBest s)) rest
                    in go (Just newBest) left (beenTo `S.union` S.fromList (newBest:beenTo1))
                else 
                    let expanded = next candidate
                        maxGeode = maybe 0 (\s -> s ^. geodeCount) bestem
                        stats = show [L.length togo, maxGeode ]
                    in trace stats $ go bestem (PQ.nub $ PQ.union rest expanded) beenTo

noHopeVs :: State -> State -> Bool
noHopeVs mx st = trace (show [mx, st]) $ mxGeodeCnt > 0 && (st == mx || wontCatchup || minute1)
     -- no point comparing against 0
    where
        geodeCnt = st ^. geodeCount
        mxGeodeCnt = mx ^. geodeCount
        wontCatchup = geodeCnt + sum [1..st ^. sRound - 1] <= mxGeodeCnt -- can't possibly catch up
        minute1 = (st ^. sRound) == 1 && (st ^. geodeRobots) <= mxGeodeCnt - geodeCnt -- not enough production to make up 

runGroup :: [Blueprint] -> Int
runGroup bps = 
    let scores = [ (idx * totalGeode bp) | (idx, bp) <- indexed ]
    in sum (scores `using` parList rdeepseq)
    where 
        indexed = zip [1..] bps
        totalGeode bp = 
            let initSt = initialState bp
                best = runBlueprint initSt
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