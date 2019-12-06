module Main (main) where

import Advent
import Data.Tree
import Data.Graph
import Control.Arrow ((&&&),(***))
import Text.Printf

main =
  do pairs <- map (toPair . words) . lines . map sep <$> getRawInput 6
     -- print pairs
     let bounds = (minimum *** maximum) $ unzip pairs
     -- print bounds
     let g = buildG bounds pairs
     -- print g
     -- print $ reachable g 4
     -- putStrLn `mapM_` (test g <$> tests)
     print $ part1 g 0
  where
    sep ')' = ' '
    sep x = x
    -- toPair ["COM",xs] = (0,fromLetter xs)
    -- toPair [xs,ys] = (fromLetter xs, fromLetter ys)
    -- fromLetter = (subtract $ fromEnum 'A') . fromEnum . head
    toPair ["COM",ys] = (0,hash3 ys)
    toPair [xs,ys] = (hash3 xs,hash3 ys)
    hash3 :: String -> Int
    hash3 [a,b,c] = fromEnum a * 10000 + fromEnum b * 100 + fromEnum c
    hash3 xs = error $ "unexpected: " ++ xs


part1 g name = sum . zipWith (\n xs -> n * length xs) [0..] . levels . head . dfs g $ [name]

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

