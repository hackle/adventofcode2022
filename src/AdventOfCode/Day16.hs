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
        sPath :: [String], -- current path
        sMinutes ::Int,     -- minutes left
        sValve :: String, -- current valve
        sLayout :: Layout, -- this gets updated when a valve is released
        sPaths :: M.Map String Paths, -- all possible paths
        sRate :: Int
    } deriving (Eq, Show)

type Input = (String, Int, [String])

toValve :: Input -> Valve
toValve (name, pressure, nexts) = Valve name pressure nexts

initialState :: [Input] -> State
initialState inp = 
    let layout = toLayout inp
    in State { sLayout = layout, sValve="AA", sPressure=0, sMinutes=30, sPath=[], sPaths = findAllPaths layout, sRate = 0 }
        
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

findAllPaths :: Layout -> M.Map String Paths
findAllPaths layout = (\Valve{vName} -> valid $ findPaths vName layout) <$> M.filter withFlowRate layout
    where
        valid = M.filterWithKey (\(_, to) _ -> withFlowRate $ layout M.! to)
        withFlowRate (Valve{vFlowRate, vName}) = vFlowRate > 0 || vName == "AA"

resetValveInPaths :: String -> Paths -> Paths
resetValveInPaths valveName paths = 
    M.mapMaybeWithKey (\(from, to) path -> if to == valveName then Nothing else (Just path)) paths 

resetValve :: String -> M.Map String Paths -> M.Map String Paths
resetValve valveName paths = resetValveInPaths valveName <$> (M.delete valveName paths)

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
round1 st = go st []
    where 
        go best left =
            -- if sMinutes state == 0 then state else
                let (best1, left1) = L.partition (\s -> sMinutes s == 0) $ step1 (traceShow (sPressure best) best)
                    (best2:left2) = L.foldl (\bag cur -> insertBagBy byPressure cur bag) left left1
                in if null best1 then go best2 left2 else head best1
        byPressure st1 st2 = (sPressure st2 ) `compare` (sPressure st1) -- <>  sMinutes st2 `compare` sMinutes st1 -- higher pressure the better

step1 :: State -> [State]
step1 st@State{sValve, sLayout, sPath, sMinutes, sPressure, sPaths} = 
    -- let minutesLeft = sMinutes - length bestValve - 1   -- 1 minute to open valve
    if M.null nextPaths
        then [st{sMinutes=sMinutes - 1}] -- no more to do, use up all minutes
        else snd <$> (M.toList $ tryOne <$> nextPaths)
        
    where 
        currentValve = sLayout M.! sValve
        nextPaths = M.mapWithKey (\(_, to) xs -> (to, sLayout M.! to, xs)) (sPaths M.! sValve)
        tryOne (nextValve, v@(Valve{vFlowRate}), path) = 
            let minutesLeft = sMinutes - length path - 1
                resetPaths = resetValve sValve sPaths
                pressure = sPressure + minutesLeft * vFlowRate
                pathUpdate = sPath ++ path ++ [nextValve]
                rate = pressure `div` length pathUpdate
            in st{
                    sPaths = resetPaths,
                    sValve = nextValve,
                    sPath = pathUpdate,
                    sPressure = pressure,
                    sMinutes = minutesLeft,
                    sLayout = M.insert nextValve (v{vFlowRate=0}) sLayout,
                    sRate = rate
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
    
trueInput :: [Input]
trueInput = 
    [
        ("EV", 0, [ "WG", "IB" ]),
        ("IB", 0, [ "EW", "EV" ]),
        ("KL", 0, [ "JH", "OY" ]),
        ("QJ", 0, [ "TX", "JH" ]),
        ("OA", 12, [ "SB", "GI", "ED"]),
        ("BQ", 0, [ "NK", "JJ" ]),
        ("PZ", 0, [ "JH", "VA" ]),
        ("QO", 8, [ "LN", "LU" , "CU", "SQ", "YZ" ]),
        ("MP", 0, [ "LN", "GO" ]),
        ("YZ", 0, [ "AA", "QO" ]),
        ("CU", 0, [ "RY", "QO" ]),
        ("UE", 16, [ "VP" ]),
        ("HT", 0, [ "AA", "JE" ]),
        ("EF", 0, [ "ES", "JE" ]),
        ("JJ", 15, [ "BQ" ]),
        ("JX", 0, [ "AA", "GO" ]),
        ("AA", 0, [ "JX", "TX" , "HT", "YZ" ]),
        ("MI", 21, [ "PQ", "QT" ]),
        ("ES", 0, [ "EF", "NK" ]),
        ("VC", 0, [ "MC", "IW" ]),
        ("LN", 0, [ "MP", "QO" ]),
        ("ED", 0, [ "OA", "RY" ]),
        ("WG", 20, [ "EV", "OY", "KF" ]),
        ("GI", 0, [ "WE", "OA" ]),
        ("UK", 0, [ "TO", "JE" ]),
        ("GY", 23, [ "EO", "QT" ]),
        ("TX", 0, [ "AA", "QJ" ]),
        ("OE", 0, [ "GO", "NK" ]),
        ("OQ", 9, [ "VP", "SB" ]),
        ("NK", 25, [ "OE", "ES", "BQ" ]),
        ("LU", 0, [ "JH", "QO" ]),
        ("RY", 18, [ "ED", "IW", "CU" ]),
        ("KF", 0, [ "JE", "WG" ]),
        ("IW", 0, [ "VC", "RY" ]),
        ("SQ", 0, [ "MC", "QO" ]),
        ("PQ", 0, [ "MC", "MI" ]),
        ("TO", 0, [ "UK", "JH" ]),
        ("OY", 0, [ "KL", "WG" ]),
        ("JE", 10, [ "EF", "ND", "HT", "KF", "UK" ]),
        ("JH", 3, [ "QJ", "KL", "PZ", "TO", "LU" ]),
        ("VP", 0, [ "OQ", "UE" ]),
        ("EW", 22, [ "IB" ]),
        ("ND", 0, [ "JE", "GO" ]),
        ("VA", 0, [ "GO", "PZ" ]),
        ("QT", 0, [ "MI", "GY" ]),
        ("EO", 0, [ "GY", "MC" ]),
        ("MC", 11, [ "PQ", "SQ", "WE", "EO", "VC" ]),
        ("GO", 4, [ "JX", "VA", "OE", "MP", "ND" ]),
        ("SB", 0, [ "OQ", "OA" ]),
        ("WE", 0, [ "MC", "GI" ])
    ]