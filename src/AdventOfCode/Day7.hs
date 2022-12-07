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
        Right files -> show $ aggregate 70000000 30000000 100000 files

aggregate tot mn mx files = (sumM $ under mx, free, required, toDelete)
    where 
        summedByPath = fmap sumFileSize files 
        aggrByPath = M.mapWithKey (\k v -> sumM $ searchByKey k summedByPath) summedByPath
        free = tot - aggrByPath M.! "/"  -- from the root dir, partial
        required = mn - free
        toDelete = 
            snd <$> M.toList aggrByPath & 
            L.sort & 
            L.find (>= required) 
        under mx = M.filter (<= mx) aggrByPath
        sumFileSize = sum . fmap snd 
        searchByKey k = M.filterWithKey (\k1 _ -> k `L.isPrefixOf` k1)
        sumM = M.foldl (+) 0
    

type Name = String
type Path = Name
type Size = Int
type File = (Name, Size)
type Files = M.Map Path [File]

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

segsToPath :: [Path] -> String
segsToPath ps = L.intercalate "/" $ reverse ps

mkPath path fname = path ++ "/" ++ fname ++ "/"

updateFiles :: [Path] -> [File] -> Files -> Files
updateFiles segs fd files = M.insert path withFullPath files where
    path = segsToPath segs
    withFullPath = fmap (\(f,s) -> (mkPath path f, s)) fd

ls :: Parser ()
ls = do
    string "ls" <* endOfLine
    fd <- many $ (file <|> dir) <* (optional endOfLine)
    (segs, files) <- getState
    putState (segs, updateFiles segs fd files)

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