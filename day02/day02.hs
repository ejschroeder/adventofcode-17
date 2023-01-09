{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (splitOn)
import Data.List (delete)

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

maxMinChecksum :: [[Int]] -> Int
maxMinChecksum lines = sum [ x - y | (x,y) <- map maximum lines `zip` map minimum lines]

divChecksum :: [[Int]] -> Int
divChecksum lines = sum $ map (\line -> head [quotient | x <- line, y <- delete x line, (quotient, 0) <- [x `divMod` y]]) lines 

main :: IO ()
main = do
  lines <- parseInput <$> readFile "input"
  print $ maxMinChecksum lines
  print $ divChecksum lines