module Main where

import AdventOfCode.Day3
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "First argument should be filename to input"
    fileName:_ -> do
      handle <- openFile fileName ReadMode
      contents <- hGetContents handle
      putStrLn $ show $ rucksack contents
      -- putStrLn $ show $ psr contents
      -- putStrLn $ show $ maxCalorie contents
