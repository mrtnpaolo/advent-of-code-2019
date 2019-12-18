{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-top-binds #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Main (main) where

import Advent
import Data.Char
import qualified Data.Set as S
import qualified Data.Map.Strict as M

main :: IO ()
main =
  do maze <- parse <$> getRawInput 18
     print (part1 maze)

type Coord = (Int,Int)
type Tile = Char

data Maze = Maze
  { entrance :: Coord
  , keys     :: M.Map Char Coord
  , doors    :: M.Map Coord Char
  , maze     :: S.Set Coord
  } deriving (Show)

data Flood = Flood
  { f_maze  :: Maze
  , f_keys  :: [Char]
  , f_dists :: M.Map Coord Int
  } deriving (Show)

parse :: String ->Maze
parse raw = Maze entrance keys doors (S.fromList $ map fst maze)
  where
    tiles = [ ((x,y),t) | (y,xs) <- zip [0..] (lines raw), (x,t) <- zip [0..] xs ]
    [entrance] = [ p | (p,'@') <- maze ]
    keys  = M.fromList [ (c,p) | (p,c) <- filter (isLower . snd) maze ]
    doors = M.fromList [ (p,c) | (p,c) <- filter (isUpper . snd) maze ]
    maze  = [ (p,t) | (p,t) <- tiles, t /= '#']

emptyFlood :: Maze -> Flood
emptyFlood m@Maze{..} = Flood
  { f_maze  = m
  , f_keys  = []
  , f_dists = M.empty }

addKey :: Char -> Flood -> Flood
addKey k f = f { f_keys = k : (f_keys f) }

-- | give each *reachable* tile a distance from a given coordinate
-- reachability is determined by the given `f_keys`
flood :: Flood -> Coord -> Flood
flood f@Flood{..} start = Flood { f_maze, f_keys, f_dists }
  where
    m@Maze{..} = f_maze

    f_dists = go (M.singleton start 0) 0 [start]

    go done d ps
      | null expand = done
      | otherwise = go done' (d+1) expand
      where
        candidates = [ q | p <- ps, q <- adj m p ]

        expand = [ q | q <- candidates
                     , q `M.notMember` done
                     , q `M.notMember` doors || toLower (doors M.! q) `elem` f_keys ]

        done' = M.union done (M.fromList [ (q,d+1) | q <- expand ])
        -- remaining' = remaining `S.difference` (M.keysSet done')

adj :: Maze -> Coord -> [Coord]
adj Maze{..} (x,y) = filter (`S.member` maze) cardinals
  where
    cardinals = [(x,y-1),(x+1,y),(x,y+1),(x-1,y)]

part1 :: Maze -> M.Map Char (Maybe Int)
part1 m@Maze{..} = M.fromList
  [ (c,n) | (c,p) <- M.assocs keys ++ map (\(x,y)->(y,x)) (M.assocs doors)
          , let n = dist p ]
  where
    f = addKey 'a' (emptyFlood m)
    Flood{..} = flood f entrance
    dist p = f_dists M.!? p
