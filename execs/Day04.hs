module Main (main) where

import Advent

import Data.List (nub)
import Data.Bifunctor (second)
-- import Data.Function
-- import Control.Arrow ((&&&))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
-- import Data.Set (Set)
-- import qualified Data.Set as S

main :: IO ()
main =
  do print $ part1 136818 685979
     print $ part2 136818 685979

part1 :: Int -> Int -> Int
part1 lower higher = length [ undefined
                            | x <- range
                            , crit1 x
                            , crit2 x
                            , crit3 x
                            , crit4 x
                            ]
  where
    range = [lower..higher]
    crit1 = (6==) . length . show
    crit2 = const True
    crit3 = any (uncurry (==)) . (zip <*> tail) . show
    crit4 = all (uncurry (<=)) . (zip <*> tail) . show

part2 :: Int -> Int -> Int
part2 lower higher = length [ undefined
                            | x <- range
                            , crit1 x
                            , crit2 x
                            , crit3 x
                            , crit4 x
                            ]
  where
    range = [lower..higher]
    crit1 = (6==) . length . show
    crit2 = const True
    crit3 n = (>0) . M.size $ candidates
      where
        digits = show n
        counts = M.fromListWith (++) . map (second pure) . rle $ digits
        candidates = M.filter ([2] ==) counts
    crit4 = all (uncurry (<=)) . (zip <*> tail) . show

rle [] = []
rle ns@(n:_) = let (xs,ys) = span (n==) ns
               in (n,length xs) : rle ys