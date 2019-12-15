module Advent
  ( module Advent
  ) where

import System.Environment (getArgs)
import Text.Printf (printf)

getRawInput :: Int {- ^ day number -} -> IO String
getRawInput n =
  do args <- getArgs
     case args of
       []    -> readFile (printf "inputs/input%02d.txt" n)
       "-":_ -> getContents
       fn:_  -> readFile fn

getIntCode :: Int {- ^ day number -} -> IO [Int]
getIntCode n = map (read::String->Int) . words . map sep <$> getRawInput n
  where
    sep ',' = ' '; sep x = x
