module Main (main) where

import Advent

main :: IO ()
main =
  do masses <- map (read :: String -> Integer) . lines <$> getRawInput 1
     print $ part1 masses
     print $ part2 masses

part1 :: Integral a => [a] -> a
part1 = sum . map fuel

part2 :: Integral a => [a] -> a
part2 = sum . map (sum . takeWhile (>0) . iterate fuel) . map fuel

fuel :: Integral a => a -> a
fuel = max 0 . subtract 2 . (`div` 3)
