module AdventOfCode.Day13 where

import Text.Parsec
import Data.List (sum, sort)

data Tree a = Leaf a | Node [Tree a] deriving (Eq, Show)

instance Ord a => Ord (Tree a) where
    compare (Leaf a) (Leaf b) = a `compare` b
    compare l@(Leaf _) n = Node [l] `compare` n
    compare n l@(Leaf _) = n `compare` Node [l]
    compare (Node []) (Node []) = EQ
    compare (Node []) (Node _) = LT
    compare (Node _) (Node []) = GT
    compare (Node (x:xs)) (Node (y:ys)) = x `compare` y <> xs `compare` ys

-- parsers!

type Parser = Parsec String ()

pLeaf :: Parser (Tree Int)
pLeaf = Leaf . (\x -> read x :: Int) <$> many1 digit

pNode :: Parser (Tree Int)
pNode = Node <$> between (char '[') (char ']') (nested `sepBy` (char ','))
    where nested = pLeaf <|> pNode

pPairs :: Parser (Tree Int, Tree Int)
pPairs = do
    tree1 <- pNode <* optional endOfLine
    tree2 <- pNode <* optional endOfLine
    pure (tree1, tree2)

withIndex = zip [1..]

round1 :: [(Tree Int, Tree Int)] -> ([(Int, Ordering)], Int)
round1 pairs = (wIdx, sumInOrder)
    where 
        ordered = (uncurry compare) <$> pairs
        wIdx = withIndex ordered
        sumInOrder = sum $ (\(idx, o) -> if o /= GT then idx else 0) <$> wIdx


dividers = [Node [Node [Leaf 2]], Node [Node [Leaf 6]]]

round2 :: [(Tree Int, Tree Int)] -> ([Tree Int], Int)
round2 pairs = (sorted, product dividerIndice)
    where 
        withDividers = dividers ++ (concat $ (\(a, b) -> [a, b]) <$> pairs)
        sorted = sort withDividers
        dividerIndice = (\(idx, tr) -> if tr `elem` dividers then idx else 1) <$> withIndex sorted

runApp raw = 
    case parse (pPairs `sepBy1` endOfLine) ":)" raw of
        Left err -> show err
        Right pairs -> show (round1 pairs, round2 pairs)

testInput =
    "[1,1,3,1,1]\n\
    \[1,1,5,1,1]\n\
    \\n\
    \[[1],[2,3,4]]\n\
    \[[1],4]\n\
    \\n\
    \[9]\n\
    \[[8,7,6]]\n\
    \\n\
    \[[4,4],4,4]\n\
    \[[4,4],4,4,4]\n\
    \\n\
    \[7,7,7,7]\n\
    \[7,7,7]\n\
    \\n\
    \[]\n\
    \[3]\n\
    \\n\
    \[[[]]]\n\
    \[[]]\n\
    \\n\
    \[1,[2,[3,[4,[5,6,7]]]],8,9]\n\
    \[1,[2,[3,[4,[5,6,0]]]],8,9]"