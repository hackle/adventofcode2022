module AdventOfCode.Day7 where

import Text.Parsec
import qualified Data.List as L
import qualified Data.Map as M
import Data.Bifunctor (bimap)
import Data.Function ((&))

totalSize :: String -> String
totalSize raw = 
    case runParser run ([], M.empty) "" raw of
        Left err -> show err
        Right files -> 
            let summed = fmap sumL files 
                startsWithKey k = M.filterWithKey (\k1 _ -> k `L.isPrefixOf` k1)
                aggr = M.mapWithKey (\k v -> M.foldl (+) 0 $ startsWithKey k summed) summed
                free = 70000000 - aggr M.! "/"
                required = 30000000 - free
                toDelete = L.find (>= required) $ L.sort $ L.map snd $ M.toList aggr
                under = M.filter (<= 100000) aggr
            in show $ (sumM under, free, required, toDelete)
    where
        sumL = sum . fmap snd 
        sumM = M.foldl (+) 0
    

type Name = String
type Path = Name
type Size = Int

type Files = M.Map Path [(Path, Size)]
type AppState = ([Path], Files)

type Parser = Parsec String AppState

prompt :: Parser String
prompt = string "$ "

cdUp :: Parser ()
cdUp = do
    string ".."
    modifyState (bimap tail id)

cdRoot :: Parser ()
cdRoot = do
    string "/"
    modifyState (bimap (const ["/"]) id)

cdIn :: Parser ()
cdIn = do
    dir <- many1 alphaNum
    modifyState (bimap (dir : ) id)

cd :: Parser ()
cd = do
    string "cd "
    cdUp <|> cdRoot <|> cdIn

file :: Parser (Name, Size)
file = do
    size <- many1 digit
    char ' '
    name <- many1 (alphaNum <|> char '.')
    return (name, read size :: Int)

dir :: Parser (Name, Size)
dir = do
    string "dir "
    name <- many1 alphaNum
    return (name, 0)

ls :: Parser ()
ls = do
    string "ls" <* endOfLine
    fd <- many $ (file <|> dir) <* (optional endOfLine)
    (segs, files) <- getState
    let path = L.intercalate "/" $ reverse segs
        makePath p = path ++ p ++ "/"
        fullPath = fmap (\(f,s) -> (makePath f, s)) fd
        files' = M.insert path fullPath files
    putState (segs, files')

command :: Parser ()
command = do
    prompt
    ls <|> cd

run :: Parser Files
run = do
    many1 (command *> optional endOfLine)
    eof
    (_, files) <- getState
    return files

testInput = "$ cd /\n\
\$ ls\n\
\dir a\n\
\14848514 b.txt\n\
\8504156 c.dat\n\
\dir d\n\
\$ cd a\n\
\$ ls\n\
\dir e\n\
\29116 f\n\
\2557 g\n\
\62596 h.lst\n\
\$ cd e\n\
\$ ls\n\
\584 i\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd d\n\
\$ ls\n\
\4060174 j\n\
\8033020 d.log\n\
\5626152 d.ext\n\
\7214296 k"