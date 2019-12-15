{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-unused-matches -Wno-unused-local-binds -Wno-incomplete-patterns #-}
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Advent
import Advent.IntCode
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable
import Control.Applicative
import Control.Monad (when)
import Control.Arrow ((***),(&&&))
import System.Random
import Debug.Trace

main :: IO ()
main =
  do mem <- getIntCode 15
     -- part1 mem
     area <- getRawInput (-1)
     putStrLn area
     part2 area >>= print
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

begin :: Game
begin = Game { pos_ = (0,0), command_ = N, area_ = M.singleton (0,0) Empty }

-- part 1

part1 :: [Int] -> IO ()
part1 mem =
  do inputs <- randomInputs
     game <- go 0 inputs begin (run' mem)
     (putStrLn . showGame) game

  where
    go i _ game (Stop)         = pure game

    go i (x:xs) game (Input f)      = do when (i `mod` 1000 == 0) $
                                           (putStrLn . showGame) game
                                         go (succ i) xs game' (f (fromEnum command))
      where
        command = x
        game' = game { command_ = command }

    go i xs game (Output n eff) =
      case (toEnum n) of
        StatusMoved -> go (succ i) xs game'' eff
          where
            game'  = moveDroid game
            game'' = areaInsert (pos_ game') Empty game'
        StatusWall  -> go (succ i) xs game' eff
          where
            game' = areaInsert (move (command_ game) (pos_ game)) Wall game
        StatusFound -> go (succ i) xs game'' eff
          where
            game'  = moveDroid game
            game'' = areaInsert (pos_ game') Goal game'

-- part 2

part2 :: String -> IO Int
part2 raw = go 0 [start] (S.fromList area)
  where
    area = [ (x,y) | (y,xs) <- zip [0..] (lines raw)
                   , (x,tile) <- zip [0..] xs
                   , tile `elem` "⌾░o" ]
    walls = [ (x,y) | (y,xs) <- zip [0..] (lines raw)
                    , (x,'▓') <- zip [0..] xs ]
    [start] = [ (x,y) | (y,xs) <- zip [0..] (lines raw)
                      , (x,'★') <- zip [0..] xs ]
    go i frontier remaining
      | S.null remaining = printRemaining >> pure i
      | otherwise        = printRemaining >> go (i+1) near far
      where
        near = [ y | x <- frontier, y <- adj x, S.member y remaining ]
        far = S.difference remaining (S.fromList near)
        printRemaining =
          do print i
             putStr (showGame game)
          where
            game = begin { area_ = M.fromList a }
            a = [ ((x,-y),Wall) | (x,y) <- walls ] ++ [ ((x,-y),Empty) | (x,y) <- toList remaining ]

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

adj :: Coord -> [Coord]
adj x = [n,e,s,w] <*> [x]

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

-- visualization

showGame :: Game -> String
showGame game = unlines $
  (flip map) [yM,yM-1..ym] $ \y ->
    (flip map) [xm..xM] $ \x ->
      case (x,y) of
        (0,0) -> 'o'
        _ | (x,y) == (pos_ game) -> '⌾'
        _     ->
          case (area_ game M.!? (x,y)) of
            Nothing -> ' '
            Just Empty -> '░'
            Just Wall  -> '▓'
            Just Goal  -> '★'
  where
    minMax = minimum &&& maximum
    ((xm,xM),(ym,yM)) = (minMax *** minMax) . unzip . M.keys . area_ $ game
