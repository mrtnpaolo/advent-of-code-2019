module Main (main) where

import Advent

-- import Data.Function
-- import Control.Arrow ((&&&))

-- import Data.Map (Map)
-- import qualified Data.Map as M
-- import Data.Set (Set)
-- import qualified Data.Set as S

main :: IO ()
main =
  do print $ part1 136818 685979
     -- print $ part2 x

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
