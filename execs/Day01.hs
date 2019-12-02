module Main (main) where

import Advent

main =
  do masses <- map (read :: String -> Integer) . lines <$> getRawInput 1
     print $ sum . map fuel $ masses
     print $ sum . map (sum . takeWhile (>0) . iterate fuel) . map fuel $ masses

fuel = subtract 2 . (`div` 3)
