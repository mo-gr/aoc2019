module Main where

import qualified AOC1
import qualified AOC2
import           System.Environment
import           System.Exit

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

format s1 s2 = do
  putStrLn "Solution 1:"
  s1 >>= print
  putStrLn "Solution 2:"
  s2 >>= print

main = do
  print "Advent of Code 2019"
  print "https://adventofcode.com/2019"
  arg <- getArgs
  case safeHead arg of
    Nothing -> putStrLn "Usage: AOC <day>" >>= const exitFailure
    Just "1" -> format AOC1.solution1 AOC1.solution2
    Just "2" -> format AOC2.solution1 AOC2.solution2
    Just other -> putStrLn $ "No sulution for day " ++ show other
