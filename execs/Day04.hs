module Main (main) where

import Advent
import Data.Bifunctor (second)
import Control.Applicative ((<**>))
import qualified Data.Map.Strict as M

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
part1 =
  [ any (uncurry (==)) . adjacents
  , all (uncurry (<=)) . adjacents ]

part2 :: [Criterion]
part2 =
  [ all (uncurry (<=)) . adjacents
  , not . M.null . M.filter ([2] ==) . M.fromListWith (++) . map (second pure) . rle ]

-- | List of pairs of adjacent elements
adjacents :: [a] -> [(a,a)]
adjacents = tail <**> zip

-- | Run Length Encoding
rle :: Eq a => [a] -> [(a,Int)]
rle [] = []
rle ns@(n:_) = let (xs,ys) = span (n==) ns
               in (n,length xs) : rle ys
