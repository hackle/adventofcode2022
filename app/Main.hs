module Main where

import AdventOfCode.Day24 ( shortest, showG, toGrid )
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "First argument should be filename to input"
    (fileName:_) -> do
      contents <- readFile fileName
      print $ show $ let g = toGrid contents; p = shortest g in (showG p g, length p )
      -- putStrLn $ show $ let State{sMemoTop=smt, sTop=st} = keepFalling (initialState (concat $ repeat jetPatterns)) 2022 in smt + fromIntegral st
      -- putStrLn $ show $ bridgeCross 10 contents
      -- putStrLn $ show $ treeHouse contents
        -- Left err -> putStrLn err
        -- Right res -> putStrLn $ show res
      -- putStrLn $ show $ rucksack contents
      -- putStrLn $ show $ psr contents
      -- putStrLn $ show $ maxCalorie contents
