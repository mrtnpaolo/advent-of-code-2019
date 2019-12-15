{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-unused-matches -Wno-unused-local-binds -Wno-incomplete-patterns #-}
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Advent
import Advent.IntCode
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import System.Random

main :: IO ()
main =
  do mem <- getIntCode 15
     part1 mem
  where
    sep ',' = ' '; sep x = x

type Coord = (Int,Int)

data Tile = Empty | Wall | Goal deriving (Show)
type Area = Map Coord Tile

data Game = Game
  { pos_     :: Coord
  , command_ :: Command
  , area_    :: Area
  } deriving (Show)

-- part 1

part1 :: [Int] -> IO ()
part1 mem =
  do inputs <- randomInputs
     game <- go inputs begin (run' mem)
     print game
  where
    go _ game (Stop)         = pure game

    go (x:xs) game (Input f)      = go xs game' (f (fromEnum command))
      where
        command = x
        game' = game { command_ = command }

    go xs game (Output n eff) =
      case (toEnum n) of
        StatusMoved -> go xs game'' eff
          where
            game'  = moveDroid game
            game'' = areaInsert (pos_ game') Empty game'
        StatusWall  -> go xs game' eff
          where
            game' = areaInsert (move (command_ game) (pos_ game)) Wall game
        StatusFound -> pure game

begin :: Game
begin = Game { pos_ = (0,0), command_ = N, area_ = M.singleton (0,0) Empty }

-- ai

randomInputs :: IO [Command]
randomInputs = map toEnum <$> randomRs (_m,_M) <$> getStdGen
  where
    _m = fromEnum (minBound :: Command)
    _M = fromEnum (maxBound :: Command)

-- movement

n, s, w, e :: Coord -> Coord
n (x,y) = (x,succ y)
s (x,y) = (x,pred y)
w (x,y) = (pred x,y)
e (x,y) = (succ x,y)

move :: Command -> Coord -> Coord
move N = n
move S = s
move W = w
move E = e

-- area

areaInsert :: Coord -> Tile -> Game -> Game
areaInsert pos tile game = game { area_ = M.insert pos tile (area_ game) }

moveDroid :: Game -> Game
moveDroid game = game { pos_ = move (command_ game) (pos_ game) }

-- commands

data Command = N | S | W | E deriving (Show)

instance Enum Command where
  toEnum 1 = N
  toEnum 2 = S
  toEnum 3 = W
  toEnum 4 = E
  fromEnum N = 1
  fromEnum S = 2
  fromEnum W = 3
  fromEnum E = 4

instance Bounded Command where
  minBound = N
  maxBound = E

data Status = StatusWall | StatusMoved | StatusFound deriving (Enum)
