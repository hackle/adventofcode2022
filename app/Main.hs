module Main where

import AdventOfCode.Day6
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "First argument should be filename to input"
    fileName:_ -> do
      contents <- readFile fileName
      putStrLn $ show $ startOfPacket contents
        -- Left err -> putStrLn err
        -- Right res -> putStrLn $ show res
      -- putStrLn $ show $ rucksack contents
      -- putStrLn $ show $ psr contents
      -- putStrLn $ show $ maxCalorie contents
