{-# Language NamedFieldPuns #-}

module AdventOfCode.Day16 where

import Data.List.Ordered
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import Debug.Trace

type ValveName = String

data Valve = 
    Valve {
       vName::ValveName, -- valve name
       vFlowRate::Int,    -- flow rate
       vNexts::[ValveName] -- leads to valves
    } deriving (Eq, Show)

type Layout = M.Map ValveName Valve

data State = 
    State {
        sPressure :: Int,    -- total pressure released
        sLayout :: Layout, -- this gets updated when a valve is released
        sValve :: String, -- current valve
        sMinutes ::Int,     -- minutes left
        sPath :: [String]
    } deriving (Eq, Show)

type Input = (String, Int, [String])

toValve :: Input -> Valve
toValve (name, pressure, nexts) = Valve name pressure nexts

initialState :: [Input] -> State
initialState inp = State { sLayout=toLayout inp, sValve="AA", sPressure=0, sMinutes=30, sPath=[] }
        
toLayout :: [Input] -> Layout
toLayout inp = M.fromList $ toKV <$> inp
    where 
        toKV i@(n, _, _) = (n, toValve i)

-- step1 :: State -> [State]
-- step1 state@(State{sLayout,sValve,sPressure,sMinutes,sPath}) = 
--     if 0 == (M.size $ M.filter (\n -> vFlowRate n > 0) sLayout)
--         then [state{sMinutes=0}]
--         else concat $ go <$> vNexts
--     where 
--         (curVal@Valve{vNexts}) = sLayout M.! sValve
--         go next = 
--             let (nv@Valve{vFlowRate}) = sLayout M.! next
--                 toStay = state{
--                     sPath= (sMinutes - 1, "_", 0):(traceShow (L.length sPath) sPath),
--                     sValve = next,
--                     sMinutes = sMinutes - 1,
--                     sRate = sPressure `div` (L.length sPath + 1)
--                 }
--                 toOpen = toStay{
--                     sPath= (sMinutes - 2, next, vFlowRate):(sMinutes - 1, "_", 0):sPath, -- book keep for each minute
--                     sLayout = M.insert next (nv{vFlowRate=0}) sLayout,    --reset pressure as it's released
--                     sPressure = sPressure + (sMinutes - 2) * vFlowRate,
--                     sMinutes = sMinutes - 2,
--                     sRate = (sPressure + (sMinutes - 2) * vFlowRate) `div` (L.length sPath + 2)
--                     }
--             in if vFlowRate > 0 && sMinutes > 0 
--                 then [toStay, toOpen] 
--                 else [toStay]

-- orderByPressure :: State -> State -> Ordering
-- orderByPressure st1 st2 = (sPressure st2) `compare` (sPressure st1) -- the higher the rate the better
--     -- ((sPressure st2) `compare` (sPressure st1)) <> ((L.length $ sPath st1) `compare` (L.length $ sPath st2))-- (valvesLeft st1) `compare` (valvesLeft st2) -- <> (sMinutes st2) `compare` (sMinutes st1) -- (sPressure st2) `compare` (sPressure st1) -- <>
--      -- <> -- the higher the pressure, the more minutes left,  the better, so reversed
--     -- (sMinutes st2) `compare` (sMinutes st1)
--     where 
--         pressureByMinute (State{sPressure, sMinutes}) = sPressure `div` sMinutes
--         valvesLeft State{sLayout} = M.foldl (+) 0 $ fmap vFlowRate sLayout

-- insertOrdered :: [State] -> State -> [State]
-- insertOrdered aggr st = insertBagBy orderByPressure st aggr

-- round1 :: State -> [State]
-- round1 initial = go idx [] [initial]
    -- where
    --     go :: Int -> [State] -> [State] -> [State]
    --     go n done [] = take 1 done
    --     go n done (cur:left) = 
    --         let nextStates = step1 cur
    --             (done1, left1) = L.partition (\st -> sMinutes st <= 0) nextStates
    --             done2 = foldl insertOrdered done1 done
    --         in go (n - 1) (done2) $ foldl insertOrdered left1 left 
        
testInput :: [Input]
testInput= 
    [
        ("AA", 0, [ "DD", "II", "BB" ]),
        ("BB", 13, [ "CC", "AA" ]),
        ("CC", 2, [ "DD", "BB" ]),
        ("DD", 20, [ "CC", "AA", "EE" ]),
        ("EE", 3, [ "FF", "DD" ]),
        ("FF", 0, [ "EE", "GG" ]),
        ("GG", 0, [ "FF", "HH" ]),
        ("HH", 22, [ "GG" ]),
        ("II", 0, [ "AA", "JJ" ]),
        ("JJ", 21, [ "II" ])
    ]

type Paths = M.Map (String, String) [String]

type Segs = [String]

findPaths :: String -> Layout -> Paths
findPaths from layout = 
    let fullPaths = findAcyclic from layout
        allPaths = concat $ filter (\xs -> length xs > 1) <$> (L.tails <$> fullPaths)
        keyed = (\xs@(x:_) -> ((from, x), tail $ reverse xs)) <$> allPaths
    in M.fromListWith (\xs ys -> if length xs <= length ys then xs else ys) keyed

findAcyclic :: String -> Layout -> [[String]]
findAcyclic from layout = go [from]
    where
        go :: [String] -> [[String]]
        go segs@(s:ss) = 
            let Valve{vNexts} = layout M.! s
                acyclic = filter (not . (`elem` segs)) vNexts
                appended = (: segs) <$> acyclic
            in if null acyclic then [segs] else concat $ go <$> appended

round1 :: State -> State
round1 st = L.maximumBy byPressure $ step1 st
    where byPressure st1 st2 = sPressure st1 `compare` sPressure st2

step1 :: State -> [State]
step1 st@State{sValve, sLayout, sPath, sMinutes, sPressure} = 
    -- let minutesLeft = sMinutes - length bestValve - 1   -- 1 minute to open valve
    if M.size withPressure == 0 
        then [st{sMinutes=0}] 
        else concat $ step1 . tryOne <$> withPressure-- no more to do, use up all minutes
        
    where 
        valve = sLayout M.! sValve
        resetBestValve = M.insert sValve (valve{vFlowRate=0}) sLayout 
        valvePaths = M.mapWithKey (\(_, to) xs -> (to, sLayout M.! to, xs)) (findPaths sValve sLayout)
        withPressure = M.filter (\(_, v, _) -> vFlowRate v > 0) valvePaths
        tryOne (valveName, v@(Valve{vFlowRate}), path) = 
            let minutesLeft = sMinutes - length path - 1
            in st{
                    sValve = valveName,
                    sPath = sPath ++ path ++ [valveName],
                    sPressure = sPressure + minutesLeft * vFlowRate,
                    sMinutes = minutesLeft,
                    sLayout = M.insert valveName (v{vFlowRate=0}) sLayout
                }

-- (bestValve, Valve{vFlowRate=bestFlowRate}, bestPath) = L.maximumBy releaseRate $ snd <$> M.toList withPressure
-- releaseRate (_, v1, path1) (_, v2, path2) = flowRateScore v1 path1 `compare` flowRateScore v2 path2

flowRateScore :: Valve -> [String] -> Int
flowRateScore Valve{vFlowRate} path = vFlowRate `div` (length path + 1)  -- takes a minute to open valve

    -- where
    --     go :: Paths -> Paths
    --     go paths = 
    --         let Valve{vNexts} = layout M.! from
    --             updatedPaths = foldl (insert1 from segs) paths vNexts
    --         in 
    --             if updatedPaths == paths 
    --                 then paths 
    --                 else M.foldlWithKey _ paths paths
    --     insert1 from segs paths to = 
    --         let key = (from, to)
    --         in maybe paths (\_ -> M.insert key (to:segs) paths) (paths M.!? key)
    