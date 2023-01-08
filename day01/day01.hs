module Main where

import Data.Char
import Data.List

sumMatches :: Int -> [Int] -> Int
sumMatches r xs = sum [ x | (x,y) <- xs `zip` drop r (cycle xs), x == y ]

main :: IO ()
main = do
  input <- map digitToInt <$> readFile "input"
  print $ sumMatches 1 input
  print $ sumMatches (length input `div` 2) input
