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

data ActorState = 
    ActorState { 
        aValve :: String,
        aPath :: [String], 
        aMinutes :: Int 
    } deriving (Eq, Show)

data State = 
    State {
        sPressure :: Int,    -- total pressure released
        -- sPath :: [String], -- current path
        sMinutes ::Int,     -- minutes left, max of both actors
        -- sValve :: String, -- current valve
        sLayout :: Layout, -- this gets updated when a valve is released
        sPaths :: M.Map String Paths, -- all possible paths
        sRate :: Float
    } deriving (Eq, Show)

data TotalState =
    TotalState { 
        tState :: State, 
        tActor1 :: ActorState, 
        tActor2 :: ActorState 
        } deriving (Eq, Show)

type Input = (String, Int, [String])

toValve :: Input -> Valve
toValve (name, pressure, nexts) = Valve name pressure nexts

totalMinutes :: Int
totalMinutes = 26

initialState :: [Input] -> TotalState
initialState inp = 
    let layout = toLayout inp
        state = State { sLayout = layout, sPressure=0, sPaths = findAllPaths layout, sRate = 0, sMinutes = totalMinutes * 2 }
        actorState = ActorState { aValve = "AA", aMinutes = totalMinutes, aPath=[] }
    in TotalState{ tState = state, tActor1 = actorState, tActor2 = actorState }
        
toLayout :: [Input] -> Layout
toLayout inp = M.fromList $ toKV <$> inp
    where 
        toKV i@(n, _, _) = (n, toValve i)

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


round1 :: TotalState -> TotalState
round1 tot = go [] [tot]
    where 
        go complete []  = head complete
        go complete (best:left) =
            let newTots = step1 (sPressure $ tState best) best
                (complete1, left1) = L.partition (\s -> sMinutes (tState s) <= 0) newTots
                completes = L.foldl (\bag cur -> insertBagBy (flip byRate) cur bag) complete complete1 -- when complete, rank by pressure
                lefts = L.foldl (\bag cur -> insertBagBy (flip byRate) cur bag) left left1  -- when searching, rank by release rate 
            in 
                case (completes, lefts) of
                    ((bestComp:_), (bestLeft:_)) -> 
                        if byRate 
                            (traceTotalState "complete" bestComp)
                            (traceTotalState "pending" bestLeft) /= GT -- there are better ones in lefts, try them out
                            then go completes lefts 
                            else bestComp   -- nothing is better in rate or pressure
                    _ -> go completes lefts

traceTotalState status tot@TotalState{tState} = trace ("best of " ++ status ++ " " ++ (show $ sPressure tState)) tot

byRate TotalState{tState=st1} TotalState{tState=st2} = 
    sRate st1 `compare` sRate st2 <> 
    sPressure st1 `compare` sPressure st2 -- <>-- higher pressure the better


idleState st@State{sPressure} = 
    st {
        sRate=(fromIntegral sPressure) / (fromIntegral $ totalMinutes * 2),
        sMinutes = 0
    }

idleActorState actorState@ActorState{aPath} = 
    let updatedPath = aPath ++ (if null aPath then [] else [last aPath])
    in actorState {
        aMinutes=0,
        aPath=updatedPath
        }

idleTotalState TotalState{tState, tActor1, tActor2} = 
    TotalState{
        tState = idleState tState,
        tActor1 = idleActorState tActor1,
        tActor2 = idleActorState tActor2
    }

selectActor actor1 actor2 = 
    if aMinutes actor1 > aMinutes actor2    -- progress the actor that's falling behind (with more minutes left)
        then traceW "actor1" (actor1, actor2, \tot val -> tot{tActor1=val}) -- value and setter
        else traceW "actor2" (actor2, actor1, \tot val -> tot{tActor2=val})
    where
        traceW name v = v --trace ("Progressing " ++ name ++ ", actors" ++ show [actor1, actor2]) v

estimate :: Int -> [Int] -> Int
estimate minutesLeft rates = 
    sum $ L.zipWith (\m fr -> m * fr) 
            (L.reverse $ L.sort rates) 
            (abs <$> [2 - minutesLeft, 4 - minutesLeft .. 0]) -- take a minute to get to the first valve then to release

bigOnesTooLate minutesLeft paths = 
    ((trace ("Time left " ++ show halfTimeGone) halfTimeGone) < 0.5 && valvesLeft >= 2) ||
    ((trace ("Time left " ++ show halfTimeGone) halfTimeGone) < 0.3 && valvesLeft >= 1) 

    where 
        halfTimeGone = ((fromIntegral minutesLeft) / (fromIntegral $ totalMinutes * 2))
        valvesLeft = M.size $ M.filter (\(_, Valve{vFlowRate}, _) -> vFlowRate >= 20) paths

step1 :: Int -> TotalState -> [TotalState]
step1 bestPressure tot@TotalState{tState, tActor1, tActor2} =
    let minutesLeft = otherActorMinutes + aMinutes
        flowRatesLeft = snd <$> (M.toList $ (\(_, Valve{vFlowRate}, _) -> vFlowRate) <$> nextPaths)
        estimatedPressure = estimate minutesLeft flowRatesLeft 
        skipForEstimate = sPressure + estimatedPressure < bestPressure
    in 
        if bigOnesTooLate minutesLeft nextPaths ||
            (trace (if not skipForEstimate then "" else "Not enough pressure to win. Current " ++ show sPressure ++ " Estimated " ++ show estimatedPressure) skipForEstimate)
         then []    -- no use trying
            else if M.null nextPaths || 
                        aMinutes < 2 ||  -- not enough time to do anything
                        otherActorMinutes < 2
                    then [idleTotalState tot] -- no more to do, just use up a minute
                    else snd <$> (M.toList $ tryOne <$> nextPaths)
        
    where 
        (actor, ActorState{aValve=otherActorValve, aMinutes=otherActorMinutes}, setActor) = selectActor tActor1 tActor2
        ActorState{ aPath, aMinutes, aValve } = actor
        State{ sLayout, sPressure, sMinutes, sPaths} = tState
        currentValve = sLayout M.! aValve
        nextPaths = 
            M.filter (\(_, _, xs) -> L.length xs + 1 < aMinutes) $ 
            M.mapWithKey (\(_, to) xs -> (to, sLayout M.! to, xs)) (sPaths M.! aValve)
        tryOne (nextValve, v@(Valve{vFlowRate}), path) = 
            let minutesLeft = aMinutes - length path - 1 -- an extra minute to open
                commonMinutesLeft = otherActorMinutes + minutesLeft -- gotta be conservative or or the actors won't finish
                resetPaths = 
                    if otherActorValve == aValve 
                        then sPaths -- the other actor still needs the paths
                        else resetValve aValve sPaths
                pressure = sPressure + minutesLeft * vFlowRate
                pathUpdate = aPath ++ path ++ [nextValve]
                rate = (fromIntegral pressure) / (fromIntegral $ totalMinutes * 2 - commonMinutesLeft)
                newState = 
                    tState{
                        sPaths = resetPaths,
                        sPressure = pressure,
                        sLayout = M.insert nextValve (v{vFlowRate=0}) sLayout,
                        sRate = rate,
                        sMinutes = commonMinutesLeft
                        }
                newActorState = 
                    actor{
                        aMinutes = traceShowId minutesLeft,
                        aPath = pathUpdate,
                        aValve = nextValve
                        }
            in setActor tot{tState=newState} newActorState

flowRateScore :: Valve -> [String] -> Int
flowRateScore Valve{vFlowRate} path = vFlowRate `div` (length path + 1)  -- takes a minute to open valve
    
trueInput :: [Input]
trueInput =
    [
        ("EV", 0, ["WG", "IB"]),
        ("IB", 0, ["EW", "EV"]),
        ("KL", 0, ["JH", "OY"]),
        ("QJ", 0, ["TX", "JH"]),
        ("OA", 12, ["SB", "GI", "ED"]),
        ("BQ", 0, ["NK", "JJ"]),
        ("PZ", 0, ["JH", "VA"]),
        ("QO", 8, ["LN", "LU", "CU", "SQ", "YZ"]),
        ("MP", 0, ["LN", "GO"]),
        ("YZ", 0, ["AA", "QO"]),
        ("CU", 0, ["RY", "QO"]),
        ("UE", 16, ["VP"]),
        ("HT", 0, ["AA", "JE"]),
        ("EF", 0, ["ES", "JE"]),
        ("JJ", 15, ["BQ"]),
        ("JX", 0, ["AA", "GO"]),
        ("AA", 0, ["JX", "TX", "HT", "YZ"]),
        ("MI", 21, ["PQ", "QT"]),
        ("ES", 0, ["EF", "NK"]),
        ("VC", 0, ["MC", "IW"]),
        ("LN", 0, ["MP", "QO"]),
        ("ED", 0, ["OA", "RY"]),
        ("WG", 20, ["EV", "OY", "KF"]),
        ("GI", 0, ["WE", "OA"]),
        ("UK", 0, ["TO", "JE"]),
        ("GY", 23, ["EO", "QT"]),
        ("TX", 0, ["AA", "QJ"]),
        ("OE", 0, ["GO", "NK"]),
        ("OQ", 9, ["VP", "SB"]),
        ("NK", 25, ["OE", "ES", "BQ"]),
        ("LU", 0, ["JH", "QO"]),
        ("RY", 18, ["ED", "IW", "CU"]),
        ("KF", 0, ["JE", "WG"]),
        ("IW", 0, ["VC", "RY"]),
        ("SQ", 0, ["MC", "QO"]),
        ("PQ", 0, ["MC", "MI"]),
        ("TO", 0, ["UK", "JH"]),
        ("OY", 0, ["KL", "WG"]),
        ("JE", 10, ["EF", "ND", "HT", "KF", "UK"]),
        ("JH", 3, ["QJ", "KL", "PZ", "TO", "LU"]),
        ("VP", 0, ["OQ", "UE"]),
        ("EW", 22, ["IB"]),
        ("ND", 0, ["JE", "GO"]),
        ("VA", 0, ["GO", "PZ"]),
        ("QT", 0, ["MI", "GY"]),
        ("EO", 0, ["GY", "MC"]),
        ("MC", 11, ["PQ", "SQ", "WE", "EO", "VC"]),
        ("GO", 4, ["JX", "VA", "OE", "MP", "ND"]),
        ("SB", 0, ["OQ", "OA"]),
        ("WE", 0, ["MC", "GI"])
    ]

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