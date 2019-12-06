module Main (main) where

import Advent
import Data.Tree
import Data.Graph
import Control.Arrow ((&&&),(***))
import Text.Printf
import Debug.Trace

main =
  do pairs <- map (toPair . words) . lines . map sep <$> getRawInput 6
     let bounds = (minimum *** maximum) $ unzip pairs
     let g = buildG bounds pairs
     print g
     putStrLn $ drawForest $ fmap (fmap show) $ dfs g [0]
     -- print $ part1 g 0
     print $ part2 g 1 2
  where
    sep ')' = ' '
    sep x = x
    toPair ["COM",ys] = (0,hash3 ys)
    toPair ["YOU",ys] = (1,hash3 ys)
    toPair [xs,"YOU"] = (hash3 xs,1)
    toPair ["SAN",ys] = (2,hash3 ys)
    toPair [xs,"SAN"] = (hash3 xs,2)
    toPair [xs,ys] = (hash3 xs,hash3 ys)
    hash3 :: String -> Int
    hash3 [x] = 2 + fromEnum x - fromEnum 'A'
    hash3 [a,b,c] = fromEnum a * 10000 + fromEnum b * 100 + fromEnum c
    hash3 xs = error $ "unexpected: " ++ xs

part1 g name = sum . zipWith (\n xs -> n * length xs) [0..] . levels . head . dfs g $ [name]

part2 g from to = abs (depth from - depth v) + abs (depth to - depth v) - 2
  where
    lvls :: [[Vertex]]
    lvls = levels . head . dfs g $ [0]
    -- v = 5 -- traceShowId $ connection g from to $ head $ dropWhile (not . connected g from to . traceShowId) (reverse lvls)
    v = head $ head $ dropWhile null [ connections g from to vs | vs <- reverse lvls ]

    depth name = sum $ zipWith (\n lvl -> if name `elem` lvl then n else 0) [0..] lvls

connections g from to vs = [ v | v <- vs, path g v from, path g v to ]

test g (name,expected) =
  printf "node: %2d\texpected: %2d\tgot: %2d\t%s" name expected got passed
    where
      got = part1 g name
      passed | expected == got = "PASSED"
             | otherwise       = "FAILED"

tests :: [(Int,Int)]
tests =
  [ (11,0), (10,1), (9,3)
  , (5,0), (4,1+2+3+1)
  , (0,42) ]

--       6 - 7       9 - 10 - 11
--      /           /
-- 0 - 1 - 2 - 3 - 4 - 5
--              \
--               8

