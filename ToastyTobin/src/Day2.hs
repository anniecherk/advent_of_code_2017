module Day2 where

import Data.List (minimum, maximum, find)
import Data.Maybe (fromJust)

run :: IO ()
run = do
  input <- getInput
  print . sum . map lineDifference $ input
  print . sum . map lineDivision $ input

lineDifference :: [Int] -> Int
lineDifference line = maximum line - minimum line

lineDivision :: [Int] -> Int
lineDivision line = uncurry quot . fromJust . find divides $ pairs
  where
    pairs = filter (not . uncurry (==)) [(a, b) | a <- line, b <- line]
    divides (a, b) = a `mod` b == 0

getInput :: IO [[Int]]
getInput = do
  raw <- readFile "resources/day-2-input"
  return . map (map read . words) . lines $ raw
