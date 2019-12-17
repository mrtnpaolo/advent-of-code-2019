module Main (main) where

import Advent
import Advent.IntCode
import Data.Char
import qualified Data.Map.Strict as M

main :: IO ()
main =
  do mem <- map (read::String->Int) . words . map sep <$> getRawInput 17
     let space = map chr (run mem [])
     putStrLn space
     let m = M.fromList [ ((x,y),c) | (y,xs) <- zip [0..] (lines space), (x,c) <- zip [0..] xs ]
     -- print m
     print (part1 m)
  where
    sep ',' = ' '; sep x = x

part1 = sum . map (uncurry (*)) . intersections

intersections m = [ (x,y) | ((x,y),'#') <- M.toList m, f (x,y) ]
  where
    f (x,y) = all ('#'==) [ tile | p <- adj (x,y), Just tile <- pure (m M.!? p) ]
    adj (x,y) = [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]
