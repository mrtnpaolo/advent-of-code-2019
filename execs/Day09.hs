{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Advent
import Advent.IntCode

main :: IO ()
main =
  do raw :: [Integer] <- map read . words . map sep <$> getRawInput 9
     print $ quine [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
     print $ run (fromIntegers raw) [1]
  where
    sep ',' = ' '
    sep x = x

    quine mem = run (fromIntegers mem) [] == mem