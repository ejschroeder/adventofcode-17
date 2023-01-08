module Main where

import Data.Char
import Data.List

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

sumMatches :: [Int] -> [Int] -> Int
sumMatches xs ys = sum $ map fst $ filter (uncurry (==)) $ zip xs ys

main :: IO ()
main = do
  input <- map digitToInt <$> readFile "input"
  print $ sumMatches input $ rotate 1 input
  print $ sumMatches input $ rotate (div (length input) 2) input
