module Day1 where

import Data.Char (isSpace)
import Data.List (splitAt)

run :: IO ()
run = do
  input <- getInput
  let rolled = tail input ++ [head input]
      halfRolled = uncurry (flip (++)) . splitAt (length input `quot` 2) $ input
  print . sum . map fst . filter (uncurry (==)) . zip rolled $ input
  print . sum . map fst . filter (uncurry (==)) . zip halfRolled $ input


getInput :: IO [Int]
getInput = map (read . pure) . filter (not . isSpace) <$> readFile "resources/day-1-input"
