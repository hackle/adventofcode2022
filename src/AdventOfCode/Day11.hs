{-# Language NamedFieldPuns, TypeApplications, BangPatterns, StrictData #-}

module AdventOfCode.Day11 where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Vector as V
import Text.Parsec

data Monkey = Monkey {
    mOp :: Integer -> Integer,
    mTarget :: Integer -> Int
    }

type MonkeyNumber = Int
type Monkeys = M.Map MonkeyNumber Monkey
type MonkeyItems = M.Map MonkeyNumber (V.Vector Integer)
-- product of primes!
type PoP = Integer
type Rounds = Int

runOneItem :: (Integer -> Integer) -> Monkey -> MonkeyItems -> Integer -> MonkeyItems
runOneItem worry m@(Monkey { mOp, mTarget }) !mItems item = M.update (Just . (V.cons finalWorry)) targetNum mItems
    where 
        targetNum = mTarget finalWorry
        finalWorry = worry $ mOp item

runOneMonkey :: (Integer -> Integer) -> MonkeyItems -> MonkeyNumber -> Monkey -> MonkeyItems
runOneMonkey worry !mItems num monkey = 
    let !done = V.foldl (runOneItem worry monkey) mItems thisItems
    in M.insert num V.empty done
    where
        !thisItems = mItems M.! num

type InspCount = M.Map MonkeyNumber Int

runRound :: (Integer -> Integer) -> Monkeys -> (InspCount, MonkeyItems) -> (InspCount, MonkeyItems)
runRound worry monkeys !st = M.foldlWithKey runOne st monkeys
    where 
        runOne (inspCnt, !mItems) num monkey = 
            let !updated = M.update (\x -> Just $ x + (V.length $ mItems M.! num)) num inspCnt
            in (updated, runOneMonkey worry mItems num monkey)

runN :: Rounds -> (a -> a) -> a -> a
runN 0 f !a = a
runN n f !a = runN (n - 1) f (f a)

states :: Rounds -> (Integer -> Integer) -> Monkeys -> MonkeyItems -> (InspCount, MonkeyItems)
states nRounds worry monkeys !mItems = runN nRounds (runRound worry monkeys) (initCnts, mItems)
    where initCnts = M.fromList $ (\n -> (n, 0)) <$> [0..(M.size mItems - 1)]

ranked !summed = L.sortOn snd $ M.toList summed

monkeyBusiness :: Rounds -> (Integer -> Integer) -> Monkeys -> MonkeyItems -> Int
monkeyBusiness nRounds worry monkeys !mItems = product $ snd <$> L.take 2 (reverse $ ranked $ fst $ allStates)
    where
        allStates :: (InspCount, MonkeyItems)
        allStates = states nRounds worry monkeys mItems

runApp :: String -> String
runApp raw = 
    case runParser pMonkeys 1 "" raw of
        Left err -> show err
        Right (!monkeys, !monkeyItems) ->
            show $ (monkeyBusiness 20 (`div` 3) monkeys monkeyItems, monkeyBusiness 10000 id monkeys monkeyItems)
        
-- parsers
-- Integer being the product of all primes
type Parser = Parsec String Integer

pMonkeyNum :: Parser Int
pMonkeyNum = do
    string "Monkey " 
    num <- many1 digit 
    char ':'
    endOfLine
    pure $ read @Int num

pItems :: Parser [Integer]
pItems = do
    spaces
    string "Starting items: "
    items <- many1 digit `sepBy1` string ", "
    endOfLine
    pure $ (read @Integer) <$> items

pOperator :: Parser (Integer -> Integer -> Integer, Integer -> Integer -> Integer)
pOperator = choice [ char '*' >> pure ((*), div), char '+' >> pure ((+), (-)) ]

pOperand :: Parser String
pOperand = many1 digit <|> string "old"

mkOperation :: (Integer -> Integer -> Integer, Integer -> Integer -> Integer) -> String -> String -> (Integer -> Integer)
mkOperation (!f, g) !op1 !op2 = let (!getOp1, !getOp2) = (numOrX op1, numOrX op2) in \x -> f (getOp1 x) (getOp2 x)
    where 
        numOrX !op = 
            if op == "old"
                then (\n -> n) 
                else let n = read @Integer op in const n

pOperation :: Parser (Integer -> Integer)
pOperation = do
    spaces
    string "Operation: new = "
    !op1 <- pOperand
    !operator <- char ' ' *> pOperator <* char ' '
    !op2 <- pOperand <* endOfLine
    let !op = mkOperation operator op1 op2
    pure op

pTarget :: Parser (Integer -> Int)
pTarget = do
    strMod <- string "  Test: divisible by " *> many1 digit <* endOfLine
    strTrue <- string "    If true: throw to monkey " *> many1 digit <* endOfLine
    strFalse <- string "    If false: throw to monkey " *> many1 digit <* optional endOfLine
    optional endOfLine
    let (nMod, nTrue, nFalse) = (read @Integer strMod, read @Int strTrue, read @Int strFalse)
    modifyState (* nMod)
    pure $ \x -> if x `mod` nMod == 0 then nTrue else nFalse

pMonkey :: Parser (Int, [Integer], Monkey)
pMonkey = do
    mNum <- pMonkeyNum
    mItems <- pItems
    mOp <- pOperation
    mTarget <- pTarget
    pure $ (mNum, mItems, Monkey {mOp, mTarget})

pMonkeys :: Parser (Monkeys, MonkeyItems)
pMonkeys = do
    tups <- many1 $ pMonkey <* optional endOfLine
    pop <- getState
    let monkeys = M.fromList $ (\(n, _, m) -> (n, m)) <$> tups
        monkeyItems = M.fromList $ (\(n, items, _) -> (n, V.fromList items)) <$> tups
    prod <- getState
    pure $ (usePop pop <$> monkeys, monkeyItems)

-- this optimises the operators
usePop :: PoP -> Monkey -> Monkey
usePop pop m@(Monkey{mOp}) = m { mOp = \x -> mOp x `mod` pop }

-- test data
testInput =
    "Monkey 0:\n\
    \  Starting items: 79, 98\n\
    \  Operation: new = old * 19\n\
    \  Test: divisible by 23\n\
    \    If true: throw to monkey 2\n\
    \    If false: throw to monkey 3\n\
    \\n\
    \Monkey 1:\n\
    \  Starting items: 54, 65, 75, 74\n\
    \  Operation: new = old + 6\n\
    \  Test: divisible by 19\n\
    \    If true: throw to monkey 2\n\
    \    If false: throw to monkey 0\n\
    \\n\
    \Monkey 2:\n\
    \  Starting items: 79, 60, 97\n\
    \  Operation: new = old * old\n\
    \  Test: divisible by 13\n\
    \    If true: throw to monkey 1\n\
    \    If false: throw to monkey 3\n\
    \\n\
    \Monkey 3:\n\
    \  Starting items: 74\n\
    \  Operation: new = old + 3\n\
    \  Test: divisible by 17\n\
    \    If true: throw to monkey 0\n\
    \    If false: throw to monkey 1"

selectTarget modNum targetTrue targetFalse n = if n `mod` modNum == 0 then targetTrue else targetFalse

testMonkeys :: Monkeys
testMonkeys = M.fromList [
    (0, Monkey { mOp = (* 19), mTarget = selectTarget 23 2 3 }),
    (1, Monkey { mOp = (+ 6), mTarget = selectTarget 19 2 0 }),
    (2, Monkey { mOp = (^ 2), mTarget = selectTarget 13 1 3 }),
    (3, Monkey { mOp = (+ 3), mTarget = selectTarget 17 0 1 })
    ]

testMonkeyItems :: MonkeyItems
testMonkeyItems = M.fromList [
    (0, V.fromList [ 79, 98 ]),
    (1, V.fromList [ 54, 65, 75, 74 ]),
    (2, V.fromList [ 79, 60, 97 ]),
    (3, V.fromList [ 74 ])
    ]