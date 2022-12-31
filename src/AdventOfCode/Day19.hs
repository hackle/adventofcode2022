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
import Control.Monad

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

data ActionHint = BAU | SkipOre | SkipClay | SkipObsidian | SkipGeo deriving (Eq, Show)

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
    _actionHint :: ActionHint,
    _firstGeodeRound :: Int,
    _simulatedGeode :: Int
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
    _actionHint = BAU,
    _firstGeodeRound = 0,
    _simulatedGeode = 0
    }

collect stOld stNew =
    stNew &
    oreCount %~ (+ stOld ^. oreRobots) &
    clayCount %~ (+ stOld ^. clayRobots) &
    obsidianCount %~ (+ stOld ^. obsidianRobots) &
    geodeCount %~ (+ stOld ^. geodeRobots) -- &
    -- (\s -> projectedGeode .~ (projection s) $ s)

decrRound st = sRound %~ (\x -> x - 1) $ st

next :: State -> [State]
next st@State{_sRound = 0 } = [st]
next st@State{_sRound = 1 } = [decrRound $ collect st st]
next st =
    case concat attempts of
        [] -> next $ collect st $ decrRound st
        sts -> L.sort $ L.nub (setSimu . collect st . decrRound <$> sts)
    where 
        setSimu st1 = simulatedGeode .~ (simulateSt st1) $ st1
        attempts =
            let geode = resetSkip <$> makeGeodeRobot st
            in if not $ L.null geode then [geode] else
                ($ st) <$> [
                    withSkip SkipObsidian makeObsidianRobot,
                    withSkip SkipClay makeClayRobot,
                    withSkip SkipOre makeOreRobot
                    ]

maxMinutes = 32

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

makeGeodeRobot :: State -> [State]
makeGeodeRobot st = 
    if tooLateToBuy then [] else
        case takeResource ore oreCount st >>= takeResource obsidian obsidianCount >>= addRobot geodeRobots of
            Nothing -> []
            Just st1 -> [firstGeodeRound %~ (max (st ^. sRound)) $ st1]   -- always build geode
    where
        (ore, obsidian) = st ^. blueprint.geodeRobotCost
        tooLateToBuy = st ^. sRound <= 1
        -- +1 to make new robot

skip ah st = actionHint .~ ah $ st
resetSkip = skip BAU
withSkip ah f st = 
    if st ^. actionHint == ah then [] else
        case f st of
            [] -> []
            sts -> skip ah st : (resetSkip <$> sts) 

-- makeObsidianRobot (3, 14) State{_oreRobots = [[8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]], _clayRobots = [], _obsidianRobots = [], _geodeRobots = [], _sRound = 17, _blueprint = BP {_oreRobotCost = 4, _clayRobotCost = 2, _obsidianRobotCost = (3,14), _geodeRobotCost = (2,7)}}
makeObsidianRobot :: State -> [State]
makeObsidianRobot st = 
    if tooLateToBuy || noExtra then [] else
        case takeResource ore oreCount st >>= takeResource clay clayCount >>= addRobot obsidianRobots of
            Nothing -> []
            Just st1 -> [st1]   -- always build obsidian
    where 
        (ore, clay) = st ^. blueprint.obsidianRobotCost
        noExtra = st ^. obsidianRobots >= st ^. blueprint.geodeRobotCost._2 
        tooLateToBuy = st ^. sRound <= 3
        -- + 1 to make new robot (this minute)
        -- + 1 to produce new obsidian
        -- + 1 to make Geode robot

makeClayRobot :: State -> [State]
makeClayRobot st = 
    if tooLateToBuy || noExtra then [] else
        case takeResource ore oreCount st >>= addRobot clayRobots of
            Nothing -> []
            Just st1 -> [st1]
    where
        ore = st ^. blueprint.clayRobotCost
        noExtra = st ^. clayRobots >= st ^. blueprint.obsidianRobotCost._2
        tooLateToBuy = st ^. sRound <= 6
        -- 1 minute to make clay robot (this minute)
        -- + 1 minute to produce a clay 
        -- + 1 minute to make an obsidian robot
        -- + 1 minute to produce obsidian
        -- + 1 minute to make a geode robot

makeOreRobot :: State -> [State]
makeOreRobot st = 
    if tooLateToBuy || noExtra then [] else
        case takeResource ore oreCount st >>= addRobot oreRobots of
            Nothing -> []
            Just st1 -> [st1]
    where
        ore = st ^. blueprint.oreRobotCost
        noExtra = st ^. oreRobots >= maximum [ st ^. blueprint.obsidianRobotCost._1, st ^. blueprint.geodeRobotCost._1, st ^. blueprint.clayRobotCost, st ^. blueprint.oreRobotCost ]
        tooLateToBuy = st ^. sRound <= 15
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

runBlueprint :: State -> State
runBlueprint state = go (sRound .~ 0 $ state) [state] S.empty
    where
        go best [] _ = error "Something terrible happened"
        go best togo beenTo = 
            let (done, unfinished) = L.span (\s -> s ^. sRound == 0) togo
                (bestest:_) = PQ.insertBag best done
                (candidate:leftYet) = unfinished    -- sorted by potential
                (left, ignored) = L.partition (\s -> not (s `S.member` beenTo || noHopeVs bestest s)) leftYet
            --  go (best:left) (beenTo `S.union` S.fromList (newBest:beenTo1))
                expanded = next candidate
                stats = show [L.length unfinished, L.length left, L.length expanded, best ^. sRound, best ^. geodeCount ]
                nextTogo = PQ.union expanded left
            in 
                if L.null unfinished 
                    then bestest 
                    else trace stats $ go bestest nextTogo (S.unions [beenTo, S.fromList done, S.fromList ignored])

noHopeVs :: State -> State -> Bool
noHopeVs mx st = wontCatchup
     -- no point comparing against 0
    where
        geodeCnt = st ^. geodeCount
        mxGeodeCnt = mx ^. geodeCount
        tooLateForGeode = geodeCnt > 0 && mxGeodeCnt > 0 && geodeCnt < mxGeodeCnt && st ^. firstGeodeRound < mx ^. firstGeodeRound
        wontCatchup = st ^. simulatedGeode <= mxGeodeCnt -- can't possibly catch up

simulateSt st = res ^. _3
    where
        res = 
            simulate 
                (st ^. obsidianCount) 
                (st ^. obsidianRobots) 
                (st ^. blueprint.geodeRobotCost._2) 
                (st ^. geodeCount) 
                (st ^. geodeRobots) 
                (st ^. sRound)

-- [15,9,0,1,8]
simulate obs obsRbts obsCost geode geodeRbts rnd =
    let obsIncr = [1..rnd]
        incr (obsAggr, rbts, geodeAggr) rnd = 
            let aggrNew = obsAggr + rbts
            in if obsAggr >= obsCost 
                then (aggrNew - obsCost, rbts + 1, geodeAggr + geodeRbts + rnd - 1) 
                else (aggrNew, rbts + 1, geodeAggr + geodeRbts)
    in foldl incr (obs, obsRbts, geode) (reverse obsIncr)

runGroup :: [Blueprint] -> Int
runGroup bps = 
    let scores = [ totalGeode bp | (idx, bp) <- indexed ]
    in product (scores `using` parList rdeepseq)
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
    BP { _oreRobotCost = 2, _clayRobotCost = 3, _obsidianRobotCost =(2, 16), _geodeRobotCost =  (2, 9) }
    -- BP { _oreRobotCost = 3, _clayRobotCost = 4, _obsidianRobotCost =(4, 20), _geodeRobotCost =  (4, 16) },
    -- BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(4, 16), _geodeRobotCost =  (2, 15) },
    -- BP { _oreRobotCost = 2, _clayRobotCost = 2, _obsidianRobotCost =(2, 20), _geodeRobotCost =  (2, 14) },
    -- BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(3, 7), _geodeRobotCost =  (3, 20) },
    -- BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(3, 14), _geodeRobotCost =  (4, 15) },
    -- BP { _oreRobotCost = 4, _clayRobotCost = 3, _obsidianRobotCost =(3, 7), _geodeRobotCost =  (2, 7) },
    -- BP { _oreRobotCost = 3, _clayRobotCost = 3, _obsidianRobotCost =(2, 11), _geodeRobotCost =  (2, 19) },
    -- BP { _oreRobotCost = 3, _clayRobotCost = 3, _obsidianRobotCost =(3, 20), _geodeRobotCost =  (2, 12) },
    -- BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(4, 20), _geodeRobotCost =  (2, 8) },
    -- BP { _oreRobotCost = 2, _clayRobotCost = 4, _obsidianRobotCost =(3, 14), _geodeRobotCost =  (4, 9) },
    -- BP { _oreRobotCost = 3, _clayRobotCost = 4, _obsidianRobotCost =(4, 18), _geodeRobotCost =  (3, 8) },
    -- BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(2, 9), _geodeRobotCost =  (3, 15) },
    -- BP { _oreRobotCost = 2, _clayRobotCost = 3, _obsidianRobotCost =(3, 11), _geodeRobotCost =  (2, 16) },
    -- BP { _oreRobotCost = 2, _clayRobotCost = 3, _obsidianRobotCost =(3, 13), _geodeRobotCost =  (3, 15) },
    -- BP { _oreRobotCost = 3, _clayRobotCost = 3, _obsidianRobotCost =(3, 16), _geodeRobotCost =  (3, 20) },
    -- BP { _oreRobotCost = 2, _clayRobotCost = 4, _obsidianRobotCost =(3, 19), _geodeRobotCost =  (4, 8) },
    -- BP { _oreRobotCost = 4, _clayRobotCost = 3, _obsidianRobotCost =(4, 16), _geodeRobotCost =  (2, 15) },
    -- BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(4, 7), _geodeRobotCost =  (2, 19) },
    -- BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(2, 14), _geodeRobotCost =  (3, 17) },
    -- BP { _oreRobotCost = 4, _clayRobotCost = 3, _obsidianRobotCost =(4, 8), _geodeRobotCost =  (2, 8) },
    -- BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(4, 7), _geodeRobotCost =  (4, 17) },
    -- BP { _oreRobotCost = 3, _clayRobotCost = 3, _obsidianRobotCost =(3, 16), _geodeRobotCost =  (3, 9) },
    -- BP { _oreRobotCost = 4, _clayRobotCost = 3, _obsidianRobotCost =(4, 15), _geodeRobotCost =  (4, 9) },
    -- BP { _oreRobotCost = 3, _clayRobotCost = 4, _obsidianRobotCost =(2, 20), _geodeRobotCost =  (4, 7) },
    -- BP { _oreRobotCost = 3, _clayRobotCost = 3, _obsidianRobotCost =(3, 17), _geodeRobotCost =  (4, 8) },
    -- BP { _oreRobotCost = 3, _clayRobotCost = 4, _obsidianRobotCost =(3, 12), _geodeRobotCost =  (3, 17) },
    -- BP { _oreRobotCost = 4, _clayRobotCost = 4, _obsidianRobotCost =(4, 5), _geodeRobotCost =  (2, 10) }
    ]