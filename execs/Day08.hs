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
     print $ length ns
     -- print `mapM_` part2 ns
     -- let ts = map (map digitToInt) . chunksOf (2*2) $ "0222112222120000"
     putStrLn `mapM_` part2 ns

part1 :: [[Int]] -> Int
part1 = (\[_,ones,twos] -> ones*twos) .map length . group . sort . minimumBy (comparing zeros)

zeros = length . filter (0==)

part2 :: [[Int]] -> [String]
part2 = showImage . map squish . transpose . reverse

squish = foldl over 2

over 2 x = x
over x 2 = x
over _ 1 = 1
over _ 0 = 0

showImage = map showLine . chunksOf 25

showLine = map showDigit

showDigit 0 = '#'
showDigit 1 = ' '
