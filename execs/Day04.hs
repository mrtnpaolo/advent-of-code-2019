module Main (main) where

import Advent
import Data.Bifunctor (second)
import Control.Applicative ((<**>))
import qualified Data.Map.Strict as M

main :: IO ()
main =
  do [lower,higher] <- map read . words . map sep <$> getRawInput 4
     print $ part1 [lower..higher]
     print $ part2 [lower..higher]
  where
    sep '-' = ' '
    sep x   = x

part1 :: [Int] -> Int
part1 = length . filter (\x -> and $ pure x <**> criterions) . map show
  where
    criterions =
      [ any (uncurry (==)) . adjacents
      , all (uncurry (<=)) . adjacents ]

part2 :: [Int] -> Int
part2 = length . filter (\x -> and $ pure x <**> criterions) . map show
  where
    criterions =
      [ all (uncurry (<=)) . adjacents
      , not . M.null . M.filter ([2] ==) . M.fromListWith (++) . map (second pure) . rle ]

adjacents :: [a] -> [(a,a)]
adjacents = tail <**> zip

-- | Run Length Encoding
rle :: Eq a => [a] -> [(a,Int)]
rle [] = []
rle ns@(n:_) = let (xs,ys) = span (n==) ns
               in (n,length xs) : rle ys
