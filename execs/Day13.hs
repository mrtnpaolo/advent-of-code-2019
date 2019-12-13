module Main (main) where

import Advent
import Advent.IntCode
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
main :: IO ()
main =
  do mem <- map (read::String->Int) . words . map sep <$> getRawInput 13
     print (part1 mem)
  where sep ',' = ' '; sep x = x

part1 mem = M.size $ go M.empty (run' mem)
  where
    go m (Output x (Output y (Output tile eff))) =
      case tile of
        2 -> go (M.insertWith (+) (x,y) 1 m) eff
        _ -> go m eff
    go m (Input f) = error "dunno"
    go m Stop = m
