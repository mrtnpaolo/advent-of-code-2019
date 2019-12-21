module Main (main) where

import Advent
import Advent.IntCode

import Data.Char

main :: IO ()
main =
  do mem <- getIntCode 21
     exec mem p "WALK"
     exec mem p' "RUN"

  where

    exec mem program command =
      do let (outs,sol) = span (<256) $ run mem (encode (program ++ [command]))
         putStrLn (ascii outs)
         print `mapM_` sol

p, p' :: [String]

p =
  [ "NOT A J"
  , "NOT B T"
  , "OR T J"
  , "NOT C T"
  , "OR T J"
  , "AND D J" ]

p' =
  [ "NOT A J"
  , "OR  D T"
  , "AND H T"
  , "OR  T J"
  , "NOT A T"
  , "NOT T T"
  , "AND B T"
  , "AND C T"
  , "NOT T T"
  , "AND T J" ]

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

-- 012345678
-- ABCDEFGHI
-- ...#...#.
-- #...#...#
-- ...##...#
-- NOT A J
-- OR  D T
-- AND H T
-- OR  T J
-- NOT A T
-- NOT T T
-- AND B T
-- AND C T
-- NOT T T
-- AND T J
