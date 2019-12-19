module Main (main) where

import Advent
import Advent.IntCode

main :: IO ()
main =
  do mem <- getIntCode 19
     let drawing = part1 mem
     putStrLn drawing
     print . length . filter ('#'==) $ drawing

part1 mem = unlines $
  (flip map) [0..49] $ \y ->
    concat $ (flip map) [0..49] $ \x ->
      map f (run mem [x,y])
  where
    f 0 = '.'
    f 1 = '#'

t = length . filter ('#'==) . unlines $
       ["#........."
       ,".#........"
       ,"..##......"
       ,"...###...."
       ,"....###..."
       ,".....####."
       ,"......####"
       ,"......####"
       ,".......###"
       ,"........##"]