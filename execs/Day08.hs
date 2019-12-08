module Main (main) where

import Advent (getRawInput)
import Data.Ord (comparing)
import Data.Char (digitToInt)
import Data.List (minimumBy, sort, group, reverse, transpose, foldl')
import Data.List.Split (chunksOf)

main :: IO ()
main =
  do ns <- map (map digitToInt) . chunksOf (25*6) . init <$> getRawInput 8
     print $ part1 ns
     putStr . showImage $ part2 ns

part1 :: [[Int]] -> Int
part1 = (\[_,ones,twos] -> ones*twos) . map length . group . sort . minimumBy (comparing zeros)

zeros :: [Int] -> Int
zeros = length . filter (0==)

part2 :: [[Int]] -> [Int]
part2 = map squish . transpose . reverse

squish :: [Int] -> Int
squish = foldl' over 2
  where
    over :: Int -> Int -> Int
    over 2 x = x
    over x 2 = x
    over _ 1 = 1
    over _ 0 = 0

showImage :: [Int] -> String
showImage = unlines . map (map showDigit) . chunksOf 25
  where
    showDigit 0 = ' '
    showDigit 1 = '#'
