module Main (main) where

import Advent
import Advent.IntCode

import Data.Char

main :: IO ()
main =
  do mem <- getIntCode 21
     let (outs,sol) = span (<256) $ run mem (encode (p ++ ["WALK"]))
     putStrLn (ascii outs)
     print `mapM_` sol

p :: [String]
p =
  [ "NOT A J"
  , "NOT B T"
  , "OR T J"
  , "NOT C T"
  , "OR T J"
  , "AND D J" ]

ascii :: [Int] -> String
ascii = map chr

encode :: [String] -> [Int]
encode = map ord . unlines

-- ABCD
-- .... ?
-- ...# !A & !B & !C
-- ..#.
-- ..##
-- .#..
-- .#.#
-- .##.
-- .###
-- #...
-- #..#
-- #.#.
-- #.##
-- ##..
-- ##.#
-- ###.
-- ####

{-

j = ( ( !a || !b ) || !c ) && d

j != a NOT A J
t != b NOT B T
j |= t OR T J
t != c NOT C T
j |= t OR T J
j &= d AND D J

-}
