module Main (main) where

import Advent
import Advent.IntCode

import qualified Data.List as L

main :: IO ()
main =
  do mem <- getIntCode 19
     print (part1 mem 50 (0,0))
     print (part2 mem 100)

part1 :: [Int] -> Int -> (Int,Int) -> Int
part1 mem size (xm,ym) = sum [ 1 | y <- [ym..ym+size-1], x <- [xm..xm+size-1], act mem (x,y) ]

part2 :: [Int] -> Int -> Int
part2 mem size = let (x,y) = walk begin in x*10000+y
  where
    begin | y <- 50, Just x <- L.findIndex (act mem) [ (x,y) | x <- [0..] ] = (x,y)

    walk (x,y)
      | act mem (x+(size-1),y-(size-1)) = (x,y-(size-1))
      | otherwise = walk (x',y')
      where
        (x',y') | act mem (x,y+1) = (x,y+1)
                | otherwise       = (x+1,y)

act :: [Int] -> (Int,Int) -> Bool
act mem (x,y) = 1 == (head $ run mem [x,y])
