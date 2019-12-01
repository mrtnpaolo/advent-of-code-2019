module Main where

main = print =<< part1 <$> readFile "input.txt"

part1 = sum . map (subtract 2 . (`div` 3)) . map (read :: String -> Int) . lines
