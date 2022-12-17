{-# Language NamedFieldPuns #-}

module AdventOfCode.Day16 where

import Data.List.Ordered
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

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
        sLayout :: Layout, -- this gets updated when a valve is released
        sValve :: String, -- current valve
        sPressure :: Int,    -- total pressure released
        sMinutes ::Int,     -- minutes left
        sPath :: [String]
    } deriving (Eq, Show)

type Input = (String, Int, [String])

toValve :: Input -> Valve
toValve (name, pressure, nexts) = Valve name pressure nexts

initialState :: [Input] -> State
initialState inp = State { sLayout=layout, sValve="AA", sPressure=0, sMinutes=30, sPath=["AA"] }
    where 
        toKV i@(n, _, _) = (n, toValve i)
        layout = M.fromList $ toKV <$> inp

step1 :: State -> [State]
step1 State{sLayout,sValve,sPressure,sMinutes,sPath} = go <$> vNexts
    where 
        (current@Valve{vFlowRate, vNexts}) = sLayout M.! sValve
        go next = State{
            sPath= next:sPath,
            sLayout = M.insert sValve (current{vFlowRate=0}) sLayout,
            sValve = next,
            sPressure = sPressure + sMinutes * vFlowRate,
            sMinutes = sMinutes - 1 - (if vFlowRate == 0 then 0 else 1)
            }

orderByPressure :: State -> State -> Ordering
orderByPressure st1 st2 = (sPressure st2) `compare` (sPressure st1) -- the higher the better, so reversed

insertOrdered :: [State] -> State -> [State]
insertOrdered aggr st = insertBagBy orderByPressure st aggr

round1 :: Int -> State -> State
round1 idx initial = go idx [initial]
    where
        go :: Int -> [State] -> State
        go 0 (h:_) = h
        go n left@(h:_) = 
            let nextStates = step1 h
                (done1, left1) = L.partition (\st -> sMinutes st == 0) nextStates
            in if null done1 
                then go (n - 1) $ foldl insertOrdered left1 left 
                else head done1
        
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