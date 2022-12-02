module Main where

import AdventOfCode
import System.Environment
import System.IO

main :: IO ()
main = mainDay1

mainDay1 :: IO ()
mainDay1 = do
  args <- getArgs
  case args of
    [] -> putStrLn "First argument should be filename to input"
    fileName:_ -> do
      handle <- openFile fileName ReadMode
      contents <- hGetContents handle
      putStrLn $ show $ maxCalorie (lines contents)
