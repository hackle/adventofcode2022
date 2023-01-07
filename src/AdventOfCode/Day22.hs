{-# Language TypeApplications #-}
{-# Language NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module AdventOfCode.Day22 where

import qualified Data.Matrix as M
import qualified Data.List as L
import Text.Parsec
import Control.Lens
import qualified Data.Vector as V
import Test.HUnit
import Debug.Trace

type MoveFn = (Int, Int) -> Int -> (Int, Int) 

data Clock = CW | CCW deriving (Eq, Show)

data Move = Turn Clock | Step Int deriving (Eq, Show)

data Direction = Upp | Rightt  | Downn | Leftt deriving (Eq, Show, Enum, Bounded) -- clockwise

data Cursor = Cursor { _sPos :: (Int, Int), _sDir :: Direction } deriving (Eq, Show)

makeLenses ''Cursor

solve rawMap rawMoves = 
    let mx = toMatrix rawMap
        Right moves = parse pMoves "unnamed" rawMoves
        cur = initCursor mx
        Cursor{_sPos=(r, c), _sDir} = eval moves mx cur
        score d = let Just x = L.findIndex (== d) [Rightt, Downn, Leftt, Upp] in x
    in r * 1000 + c * 4 + (score _sDir)

startPos :: M.Matrix Char -> (Int, Int)
startPos mx = 
    let Just c = V.findIndex (== '.') $ M.getRow 1 mx
    in (1, c + 1)   -- matrix is 0 based

initCursor mx = Cursor { _sPos = (startPos mx), _sDir = Rightt }

eval :: [Move] -> M.Matrix Char -> Cursor -> Cursor
eval [] _ cur = cur
eval (m:ms) mx cur = 
    case m of 
        Turn clk -> eval ms mx (sDir %~ makeTurn clk $ cur)
        Step n -> let c1 = takeSteps (traceShowId cur) (trace ("Take " ++ show n ++ " steps") n) mx in eval ms mx c1

takeSteps :: Cursor -> Int -> M.Matrix Char -> Cursor
takeSteps cur n mx = sPos .~ stepped $ cur 
    where 
        pos = cur ^. sPos
        direction = cur ^. sDir
        (r, c) = cur ^. sPos
        col = V.toList $ M.getCol c mx
        row = V.toList $ M.getRow r mx
        go xs idx0 incr setter = 
            let (idx, path) = traceShowId $ extractPath idx0 xs 
                idx1 = translateWS xs $ trace ("After taking steps ") $ traceShowId $ stepN path idx incr
            in setter .~ idx1 $ pos
        stepped = 
            case direction of
                Downn -> go col r n _1
                    -- let (idx, path) = extractPath r col 
                    --     r1 = translateWS col $ stepN path idx n
                    -- in (r1, c)
                Upp -> go col r (0 - n) _1
                    -- let (idx, path) = extractPath r col 
                    --     r1 = translateWS col $ stepN path idx (0 - n)
                    -- in (r1, c)
                Rightt -> go row c n _2
                    -- let (idx, path) = traceShowId $ extractPath c row 
                    --     c1 = traceShowId $ translateWS row $ stepN path idx n
                    -- in (r, c1)
                Leftt -> go row c (0 - n) _2
                    -- let (idx, path) = extractPath c row 
                    --     c1 = translateWS row $ stepN path idx (0 - n)
                    -- in (r, c1)

-- checkSteps :: M.Matrix Char -> Cursor -> Int -> Int
-- checkSteps mx cur n = min maxN n `mod` len
--     where
--         maxN = 1
--         (r, c) = cur ^. sPos
--         col = V.toList $ M.getCol r mx
--         row = V.toList $ M.getCol c mx
--         (pathWS, path) = 
--         len = L.length path 

testExtractPath = [
    TestCase(assertEqual "Empty" (0, "") (extractPath 1 ""))
    ,TestCase(assertEqual "No whitespace" (0, "...") (extractPath 1 "..."))
    ,TestCase(assertEqual "Leading whitespaces" (1, "...") (extractPath 4 "  ..."))
    ,TestCase(assertEqual "Leading whitespaces" (0, "...") (extractPath 3 "  ..."))
    ,TestCase(assertEqual "Leading and trailing whitespaces" (0, "...") (extractPath 3 "  ...  "))
    ]
extractPath idx xs = 
    let (leadingWS, rest) = L.span (== ' ') xs
        (path, _) = L.break (== ' ') rest
    in (idx - 1 - L.length leadingWS, path) -- -1 for Vector is 0 based Matrix is 1

testTranslate = [
    TestCase(assertEqual "No whitespace" 1 (translateWS "." 0))
    ,TestCase(assertEqual "No whitespace non-0" 3 (translateWS "..." 2))
    ,TestCase(assertEqual "Trailing whitespace" 3 (translateWS "...   " 2))
    ,TestCase(assertEqual "2 Leading whitespaces" 5 (translateWS "  ...   " 2))
    ]
translateWS xs idx = idx + L.length ws + 1   -- +1 for Vector is 0 based Matrix is 1
    where (ws, _) = L.span (==' ') xs

testStepN = [
    TestCase(assertEqual "Single element" 0 (stepN "+" 0 80))
    ,TestCase(assertEqual "Free range both ways" 4 (stepN "....+...." 4 18))
    ,TestCase(assertEqual "0 step" 4 (stepN "....+..." 4 0))
    ,TestCase(assertEqual "one step" 5 (stepN "....+..." 4 1))
    ,TestCase(assertEqual "to the end" 0 (stepN "....+..." 4 4))
    ,TestCase(assertEqual "wrap 1" 1 (stepN "....+..." 4 5))
    ,TestCase(assertEqual "steps to end but blocked" 6 (stepN "....+..#" 4 4))
    ,TestCase(assertEqual "steps over but blocked at the end" 6 (stepN "....+..#" 4 18))
    ,TestCase(assertEqual "blocked right after wrapping" 7 (stepN "#...+..." 4 18))
    ,TestCase(assertEqual "blocked after wrapping" 0 (stepN ".#..+..." 4 18))
    ,TestCase(assertEqual "blocked after wrapping right before current" 2 (stepN "...#+..." 4 18))
    ,TestCase(assertEqual "backwards" 3 (stepN "....+..." 4 (-1)))
    ,TestCase(assertEqual "backwards blocked" 4 (stepN "...#+..." 4 (-1)))
    ,TestCase(assertEqual "backwards free to beginning" 0 (stepN "....+..." 4 (-4)))
    ,TestCase(assertEqual "backwards wrap" 7 (stepN "....+..." 4 (-5)))
    ,TestCase(assertEqual "backwards free circle" 4 (stepN "....+..." 4 (-8)))
    ,TestCase(assertEqual "backwards blocked right after wrap" 0 (stepN "....+..#" 4 (-18)))
    ,TestCase(assertEqual "backwards blocked after wrap" 6 (stepN "....+#..." 4 (-18)))
    ,TestCase(assertEqual "free ahead but bounded test" 5 (stepN ".......#...." 0 5))
    ,TestCase(assertEqual "somehow up going is negative" 7 (stepN "......#......................#...................." 21 (-34)))
    ]
stepN xs idx n = 
    case (aheadFree == ahead, behind == behindFree) of
        (True, True) -> (idx + n) `mod` length xs
        (False, _) -> 
            let maxN = sign * length aheadFree
                boundedN = bound n maxN
            in idx + boundedN
        (True, _) -> 
            let maxN = sign * (length aheadFree + length behindFree)
                boundedN = bound n maxN
            in (idx + boundedN) `mod` length xs
    where
        (sign, bound) = if n < 0 then ((-1), max) else (1, min)
        (behind, ahead) = let (b, _:a) = L.splitAt idx xs in if n > 0 then (b, a) else (reverse a, reverse b)
        [aheadFree, behindFree] = fst . span (== '.') <$> [ ahead, behind ]

toMatrix raw = 
    let ls = lines raw
        mxLen = L.maximum $ length <$> ls
        padRight l = l ++ replicate (mxLen - length l) ' '
        withEvenLength = padRight <$> ls
    in M.fromLists withEvenLength

makeTurn :: Clock -> Direction -> Direction
makeTurn CW Leftt = Upp
makeTurn CW d = succ d
makeTurn CCW Upp = Leftt
makeTurn CCW d = pred d

type Parser = Parsec String ()

pTurn :: Parser Move
pTurn = Turn <$> (cw <|> ccw)
    where
        cw = char 'R' *> pure CW
        ccw = char 'L' *> pure CCW

pSteps :: Parser Move
pSteps = Step . (read @Int) <$> many1 digit

pMoves :: Parser [Move]
pMoves = many1 (pSteps <|> pTurn)

tests = concat [ testStepN, testTranslate, testExtractPath ]

testMoves = "10R5L5R10L4R5L5"

testInput = 
    "        ...#\n\
    \        .#..\n\
    \        #...\n\
    \        ....\n\
    \...#.......#\n\
    \........#...\n\
    \..#....#....\n\
    \..........#.\n\
    \        ...#....\n\
    \        .....#..\n\
    \        .#......\n\
    \        ......#."

prodMoves="22R2L18R32R4L40R7R49L28R47R49L13L38R34R4R5R18L3L3L8L26L14R17L12L7R24R22L15L12L24L18R11R23R36R42L27L33R15L45L34R4R23L43L27R28R47R46R16L32L37L33L34L4L11L14R28L24R25R23R28R26L47L36L27R42R43R3L4R8R16R36R10R1R30L32L39R34R20R47L44R36R38R7L19R15L5R10R41L7L6R44R26L1L14L38L11R37R15L26L3R5L3R42R46R43L18R24L30L1R10L41L48L28R24R9L50R20R15L36R17R11L50R44R50L33R8R24R46L40R35L7L19R25L25L31R5R8L15R37L6L4L47R48L49R10R34L8R22R7R41R37L45L13L48L23L1R47R27R3L37L46R36L16R19L36R5L28R10L40L41R36R18L25L18L48L3R26R37L22R16R4R2R33R11R31R16R13L13L27L22R24L35R50R20R25R16L39R33L26R18L24R30R34L43L39L19R21L32L18L12L47R25L40L38R30L31R3R26R35L40R15L12R22L29L16L41L41R9L36R30R6L17L24L13L6L23L17L15R47R14L37R37L49R5L24L30R7R1R30L45L21L3L12R41R26R42R42R49L50R37L18L16L25R24R30R21L9L24L36L18R21R46R22R38R12R30L40L21L1R4L31L43L42R2R6R20R30R47R44R47R29R39L13L44L17R8L21R41L26L49R11L20L32R9R42R1L12L15R49R11R16R35L13L7L4R42R45L11R24R41L4R16L19R39R43R49R49R10R39R42L11L25L25R46R47L50R44L4L38R47L5L9R41R33R50L20R50R16L38R41R43R34R28L39R7R25L50L5L49L42R26L9L6L38L36L13R17L29R23R2L36L33R32L48L26L31R28L7R37L7L9L14R21L48R16L2L21L10R29R48L37R35L23R42L11R44L41L8L40R21L45R26L10R7R21R11R26R13R46R33R8L47R17L45L12L31L48L2R48R43L28R6R31R8R31L16L25L34R47L23L37L41L38L3L41L50L30R24L8R41L38L26L43L43R41L3L25R32R28R12L44R34R5R4L50L45R39L5L46L13L11L5R40L20L47R26L12L26R49L8L14L32R4R3R10L11R18L16L19R39L44L19L30L48R5L12L42R38L3L16L32R45L43R21R47R3L45R47R2L23R33L43R44R13R49R12L37R26R7L43R8L30L38L13L28R1L41R28R20R38R6L40R26L9R18R25L33L3R13L24L33L18L43L47L48R29R43L36R44R35R7L50L33L35L40R2R31L32R43L50R8L24R8L8R46R11R1R5R28R24R31R42L45R42R44R17L28L11L36R13L33R29L34L4R49R13L13L25L17R24R21R35L5L29L32L19R32R22R36R26R4L39R25R29R15L14R31L28L8L16L40R10L48L27R22L33L11R37L17L39R36R46R34L47R6R31R40R21R11L14R49R45R33L35L39R4L4L41L49L5L15R21L21L20L43L31L28R41L42R48R23R9L9R4L25R37R38R10L39L15L21L19L6R32L41R21L6L23R5L20L42L45R15L36R5R14L10L34R49R40L47L15L50R23R34R6R24R20R11R10R43L9R46L45L45R12R43L46L43R45L15R14L49L7R25R22L47L8L30R40R8R42L46L13L11L5R10L40L2L28R27L19R43L45R23R6L8R14L30R31R50L6L7L33L29L19L25L29L9R41L19R9L45R18L25R8L22L2R22L12L46R15L5L11L46R34R47L15L49L29R18R14R26R9L25R30R34L43R43L44L6L19R7L49R28L12L35L9R23L24L35R49R46R29L13R49L25L4L49L41L41L39L16R5L47R28L34R8R2R47R41R48R10R21L14L28L19L27R22R34R11L9R7R11L40R45L21L1L31R10L35L24R6L1L47R7L4L17R39R44R10R3R24L39L17R9L44R2R6R14L1L28L36R35L7L23L25R18L12L18L14L8L35R4L49R50R45R46L10L37R11R18R45R33R6R19R31R15R42L47L4R31L8L9R10L7R4R9L31L32R1L26R13R39R14R18L16L8R50R16R7L49L25L14R27R29R40L40L1L29L12L11L1L8R4R14R29L3R11L49R5R29L26R28R6L30R49L3R49L10L23R38R2R9R21L21L2L29L8L10R34R3L2L23L38R32L48L11R6R26R44L39R9R40R43R22R19R2R17R11L16L42R28L23R1L39R11R12R1L6R32L24R7L19R41R18R35L37R26L27R16L31R45L21L30L34L39L30L50R43L42R44R2R40L14R11R13L45L14L21R49R25L32R40R50L20R3L24R38R14R2R37L7L1L18L21R2R23L4L36L10R46R11L43R4L1R20L10L16L11R41L43L48R3R15L32R39R48L19L9L26R23R21R9L18R47R23R3L44L36R19R42R16L4L47R44R48L8R41L4L34R2L2L37R18L40R10R46L1L1R12R44R28R30R32L25R17L13L35R30L4L4R3L33R10R24R47L47R14R20L49L25L25L21R22R2R3R8L28R27R34L48L41L26R10R47R17R47L43R44L22R45R21L6R22R6L26L47L32R43L25R18L39R30R36R40R17L9R32L6R25L49L32L12L6L16L14R41R44R15R15R25R20R6R50L24R19R13L33L36L10R11R47R18L25L49L17R40R33L46R2R22R34R13R36L8L17R36R20R47R16R37L7L19L39R44R43R22L41L10R30L22L1R19L1L48L8R39L48L6R5L23R21L12R8L11L33R48R25R31R11L5R49L12R28L27R16R22R47L23L47R28R9L16L25R45L42L32L48L5R27L49R17L1R6L43L23L44L26L9L17R42L10R49R49L48R19L10R40R13R20L10R9L46R31R31L49L3L45L41R9L33L27R25R34L15R6L40L46L7R17R14R22R45L20L17R10L3L12R18L36L43L45L33R46R39R10L19L5R42R8R7R8R28R8L27R41R30R28R9L14R41R45L19R27L46R35L14R23L36R50R37R11R40R23L31L1R26R14L18L8R43L11L44L25L30L43L37R44L41L25R6L27L37L47R20R14L47R5R47R44R17R24L49R16L47L16L16R44L38L26L15L2L43L47R38R37R23L12L38R41R48R8L35R4R9R22R23L47R16L24L19R28R8L13L44R12R4L8L20R32R48R19R13L3R15R7L45R1R27R50R40L45R37R6L46L48R34L14R17L36L24R19L31R23L44R1R12R12L33R31R4L35L7R44L24R13R5R32R4L11R33L15L21R28R31L45R3R25L44L2R22L47L47L10L3R33L37L39L31L24R41L44R29R16R6L24R13R49L43R26L13R6L15L14L25L24L39R45R9R9R37R10R37R42R22L29R6L29R42R37L29L9L37L37L38R35L33L22R9L26R7R33L12R2R50L43L29L40R36L17L50L37R2L20R14L15L27R3R1L41R26R4L49R6R5R36L9R7L8L13L27L9L7R22L38L28R45L41L17L41L40L34L44R23L50R7R31R3R12L47L2R49R11L9R39L14R47L31R6R30L22R18R9R26R22R43L33L29L34L50L19R37R12R10L9R39L14R29L39R7L2R21L37R41L20R4L37L43R34L6R6R12R39R9L14L41R11R36L21L28L37L50L14L36R9L37R40R10R6R33L11R15L5L1L34L35L46R3R39R16R18R40R31L31R14L14L34L43R25R8R43R7L23L41R40L11R12R46R47R14L44L26L13L5L37R10L10R8L23L34R11L48R13R17R45R34R22L48L18L48R44L21R8L41L48R3L26L27R3L16R12L4R32L50R34R45R11L14R33L47L12R41R24R29R32R45L14R22R48R45R3R7R13R46L8L3L46L4L23L33L14L9L1R33R11L31R22L13R33R28R35L2R8R12L2L16L40L48L24R27L38R7L14R35R49L22R36L28R35L31L14R13L32L7L45R1L15R28R46L33R12R35R44R11L48R7L23R1L16L8R20R15R22L22L28R47R12R34R30L36L41R31R19L4R26L19L24L7L48R8R7R11L39L17L30L26L37L8L47L23L35R26R47L47L34L29R41L7L31R50L16L27L21L10L28L24R22L47L29R14L13L22L20L47R17R6R8R17R38R26L41L11L4R49L16L50L11R13L10L44R48R25L29R27L17R50L37R10R7L12R17L35L15L49R5R4R17L31R8L23R20R20L28L27R12L48L19L4R41L50L45L21R24R10L27L6L6R36L49L24R31R2R50R20R18R2L42R34L32R47R6L25L22R35L45R23R1L23R15R44L31L18R25R30L6L31L37L1L19R46R38R19R29R16R23R13R18L44R38R27R6R11R14L27L30R47L10R21R37L8L29L14L36R26L15R37L7L25R50R11L30R4L25R13L9R42L28R16R26R23R4R16R20L29R43R8L12R18L8R37L1L46R9R47L50L44R10R46L48L45L35R42R40L43L17L37R40L25R13R26L19L11L50R45R37R13L15R18R19L21L30R37R42L24L47R48L7L8R25R22L14R17R12L24R42L35L10L28R17R18L9L19L46R10L39L49L19L1L44R12L2R23R41R39L43R21R13L42R3L31R9L33R22R21L42L38R37R26R49L6R28L28L25L9L3L21R2R3L45L36L36L21L32L31R37L41L44L17L5L36L18L23L19L48R1R28R23L6L12L48"