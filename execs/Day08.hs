module Main (main) where

import Advent
import Data.Ord
import Data.Char (digitToInt)
import Data.List
import Data.List.Split
import Data.Function

main :: IO ()
main =
  do ns <- map (map digitToInt) . chunksOf (25*6) . init <$> getRawInput 8
     print $ part1 ns
  where
    sep x = x

-- part1 :: [Int] -> Int
part1 = (\[_,ones,twos] -> ones*twos) .map length . group . sort . minimumBy (comparing zeros)

zeros = length . filter (0==)