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
  , keys :: M.Map Char Coord
  , doors :: M.Map Char Coord
  , maze :: S.Set Coord
  } deriving (Show)

parse :: String ->Maze
parse raw = Maze entrance keys doors (S.fromList $ map fst maze)
  where
    tiles = [ ((x,y),t) | (y,xs) <- zip [0..] (lines raw), (x,t) <- zip [0..] xs ]
    [entrance] = [ p | (p,'@') <- maze ]
    keys  = M.fromList [ (c,p) | (p,c) <- filter (isLower . snd) maze ]
    doors = M.fromList [ (c,p) | (p,c) <- filter (isUpper . snd) maze ]
    maze  = [ (p,t) | (p,t) <- tiles, t /= '#']

-- | give each inside tile a distance from a given coordinate
flood :: Maze -> Coord -> M.Map Coord Int
flood m@Maze{..} start = go (S.delete start maze) (M.singleton start 0) 0 [start]
  where
    go remaining done d ps
      | S.null remaining = done
      | otherwise = go remaining' done' (d+1) expand
      where
        expand = [ q | p <- ps, q <- adj m p, q `M.notMember` done ]
        done' = M.union done (M.fromList [ (q,d+1) | q <- expand ])
        remaining' = remaining `S.difference` (M.keysSet done')

adj :: Maze -> Coord -> [Coord]
adj Maze{..} (x,y) = filter (`S.member` maze) cardinals
  where
    cardinals = [(x,y-1),(x+1,y),(x,y+1),(x-1,y)]

part1 :: Maze -> M.Map Char Int
part1 m@Maze{..} = M.fromList
  [ (c,n) | (c,p) <- M.assocs keys ++ M.assocs doors
          , let n = dist p ]
  where
    dists = flood m entrance
    dist p = dists M.! p
