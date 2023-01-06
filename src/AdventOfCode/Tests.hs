module Main (main) where

import Test.HUnit
import System.Exit

import qualified AdventOfCode.Day22 as D22

main = do    
    counts <- runTestTT ( test $ concat [
        D22.tests
        ])
    if (errors counts + failures counts == 0)
        then exitSuccess
        else exitFailure