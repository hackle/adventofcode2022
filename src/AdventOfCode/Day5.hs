module AdventOfCode.Day5 where

import Text.Parsec
import Data.List
import Data.Function ((&))
import Data.Either.Combinators (mapLeft)
import qualified Data.Map as M
import Data.List.Index

type Parser = Parsec String ()

supplyStacks :: String -> String
supplyStacks raw = 
    case (parse task "" raw) of 
        Left err -> show err
        Right (rStacks, rMoves) -> show [ tops $ runMoves reverse rStacks rMoves, tops $ runMoves id rStacks rMoves ]

type Stack = [Char]
type Stacks = M.Map Int Stack
data Move = Move {
    mCount :: Int,
    mFrom  :: Int,
    mTo    :: Int
    } deriving (Show, Eq)

tops :: Stacks -> [Char]
tops = fmap head . fmap snd . M.toList

runMoves :: ([Char] -> [Char]) -> Stacks -> [Move] -> Stacks
runMoves order stks ms = foldl (move1 order) stks ms

move1 :: ([Char] -> [Char]) -> Stacks -> Move -> Stacks
move1 order stacks (Move cnt frm to) = 
    M.insert frm rest (M.update (Just . ((order toMove) ++)) to stacks)
    where 
        (toMove, rest) = splitAt cnt fromStack
        fromStack = stacks M.! frm

crate :: Parser Char
crate = between (char '[') (char ']') letter

emptyCrate :: Parser Char
emptyCrate = count 3 (char ' ') *> pure ' '

-- parse crates "" "[a] [b]"
crates = (try emptyCrate <|> crate) `sepBy1` (char ' ')


toMap :: [Stack] -> Stacks 
toMap xs = M.fromList $ imap (\i a -> (i + 1, a)) xs

-- ghci> parse stacks "" "[a]     [b]\n[d] [e] [f]"
-- Right ["ad","e","bf"]
stacks :: Parser Stacks
stacks = many (crates <* endOfLine) & 
    fmap transpose &
    fmap (\xs -> toMap $ fmap noWhiteSpaces xs)
    

noWhiteSpaces :: String -> String
noWhiteSpaces = filter (/= ' ')

index :: Parser String
index = (char ' ' *> digit <* char ' ') `sepBy` (char ' ')

move :: Parser Move
move = do
    string "move "
    cnt <- many1 digit
    string " from "
    fromIdx <- many1 digit
    string " to "
    toIdx <- many1 digit
    return $ Move (read cnt :: Int) (read fromIdx :: Int) (read toIdx :: Int)

moves = move `sepBy` endOfLine

task :: Parser (Stacks, [Move])
task = do
    rStacks <- stacks
    index <* endOfLine
    many (char ' ') <* endOfLine
    rMoves <- moves
    eof
    return (rStacks, rMoves)

-- round1 :: [String] -> Either String String
-- round1 raw = do
--     stacks <- parse "" pStacks sStacks
--     moves <- parse "" pMoves sMoves
--     return $ foldl runMove moves stacks 

testInput = "    [D]    \n\
            \[N] [C]    \n\
            \[Z] [M] [P]\n\
            \ 1   2   3 \n\
            \\n\
            \move 1 from 2 to 1\n\
            \move 3 from 1 to 3\n\
            \move 2 from 2 to 1\n\
            \move 1 from 1 to 2"

testStacks :: M.Map Int Stack
testStacks = M.fromList [(1,"NZ"),(2,"DCM"),(3,"P")]
testMoves = [
    Move {mCount = 1, mFrom = 2, mTo = 1},
    Move {mCount = 3, mFrom = 1, mTo = 3},
    Move {mCount = 2, mFrom = 2, mTo = 1},
    Move {mCount = 1, mFrom = 1, mTo = 2}
    ]