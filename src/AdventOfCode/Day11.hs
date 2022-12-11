{-# Language NamedFieldPuns #-}

module AdventOfCode.Day11 where

import qualified Data.Map as M
import qualified Data.List as L

data Monkey = Monkey {
    mOp :: Int -> Int,
    mTarget :: Int -> Int
    }

type MonkeyNumber = Int
type Monkeys = M.Map MonkeyNumber Monkey
type MonkeyItems = M.Map MonkeyNumber [Int]

selectTarget modNum targetTrue targetFalse n = if n `mod` modNum == 0 then targetTrue else targetFalse

monkeys :: Monkeys
monkeys = M.fromList [
    (0, Monkey { mOp = (* 19), mTarget = selectTarget 23 2 3 }),
    (1, Monkey { mOp = (+ 6), mTarget = selectTarget 19 2 0 }),
    (2, Monkey { mOp = (^ 2), mTarget = selectTarget 13 1 3 }),
    (3, Monkey { mOp = (+ 3), mTarget = selectTarget 17 0 1 })
    ]

testMonkeyItems :: MonkeyItems
testMonkeyItems = M.fromList [
    (0, [ 79, 98 ]),
    (1, [ 54, 65, 75, 74 ]),
    (2, [ 79, 60, 97 ]),
    (3, [ 74 ])
    ]

runOneItem :: Monkey -> MonkeyItems -> Int -> MonkeyItems
runOneItem m@(Monkey { mOp, mTarget }) mItems item = M.update (Just . (++ [finalItem])) targetNum mItems
    where 
        finalItem = mOp item `div` 3
        targetNum = mTarget finalItem

runOneMonkey :: MonkeyItems -> MonkeyNumber -> MonkeyItems
runOneMonkey mItems num = M.insert num [] $ foldl (runOneItem $ monkeys M.! num) mItems thisItems
    where
        monkey = monkeys M.! num
        thisItems = mItems M.! num

runRound :: MonkeyItems -> MonkeyItems
runRound mItems = foldl runOneMonkey mItems [0..3]

states nRounds mItems = L.take nRounds $ iterate runRound mItems

summed states = L.foldl1 (M.unionWith (+)) $ fmap (fmap L.length) states

ranked summed = L.sortOn snd $ M.toList summed

monkeyBusiness :: Int -> MonkeyItems -> Int
monkeyBusiness nRounds mItems = product $ snd <$> L.drop 2 (ranked . summed . states nRounds $ mItems)
        
        
        