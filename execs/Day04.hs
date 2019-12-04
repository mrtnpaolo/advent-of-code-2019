module Main (main) where

import Advent
import Data.List (group)
import Control.Applicative ((<**>))

main :: IO ()
main =
  do [lower,higher] <- map read . words . map sep <$> getRawInput 4
     print $ satisfying part1 [lower..higher]
     print $ satisfying part2 [lower..higher]
  where
    sep '-' = ' '
    sep x   = x

type Criterion = String -> Bool

satisfying :: [Criterion] -> [Int] -> Int
satisfying cs = length . filter (\x -> and $ pure (show x) <**> cs)

part1 :: [Criterion]
part1 = [ nondecreasing, any (1 <) . runs ]

part2 :: [Criterion]
part2 = [ nondecreasing, any (2 ==) . runs ]

nondecreasing :: Ord a => [a] -> Bool
nondecreasing = all (uncurry (<=)) . adjacents

-- | List of pairs of adjacent elements
adjacents :: [a] -> [(a,a)]
adjacents = zip <*> tail

-- | The runs part of run-length encoding
runs :: Eq a => [a] -> [Int]
runs = map length . group
