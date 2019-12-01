module Main where

main =
  do masses <- map (read :: String -> Integer) . lines <$> readFile "inputs/input01.txt"
     print $ sum . map fuel $ masses
     print $ sum . map (sum . takeWhile (>0) . iterate fuel) . map fuel $ masses

fuel = subtract 2 . (`div` 3)
