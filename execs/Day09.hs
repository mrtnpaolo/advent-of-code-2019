{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Advent
import Advent.IntCode

main :: IO ()
main =
  do mem <- map read . words . map sep <$> getRawInput 9
     -- runTests
     print (run mem [1])
     print (run mem [2])
  where
    sep ',' = ' '
    sep x = x

runTests :: IO ()
runTests = mapM_ print [
    quine [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99],
    16 == (length . show . head $ run [1102,34915192,34915192,7,4,7,99,0] []),
    1125899906842624 == (head $ run [104,1125899906842624,99] [])
  ]
  where
    quine mem = run mem [] == mem
