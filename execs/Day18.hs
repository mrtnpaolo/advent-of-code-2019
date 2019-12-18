{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-top-binds #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Main (main) where

import Advent
import Data.Char
import Data.Maybe
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M

import Debug.Trace

main :: IO ()
main =
  do maze <- parse <$> getRawInput 18
     let solutions = part1 maze
     print solutions
     print (minimum solutions)

type Coord = (Int,Int)
type Tile = Char

data Maze = Maze
  { entrance :: Coord
  , keys     :: M.Map Char Coord
  , doors    :: M.Map Coord Char
  , maze     :: S.Set Coord
  } deriving (Show)

type Flood = M.Map Coord Int
-- data Flood = Flood
--   { f_maze  :: Maze
--   , f_keys  :: [Char]
--   , f_dists :: M.Map Coord Int
--   } deriving (Show)

parse :: String ->Maze
parse raw = Maze entrance keys doors (S.fromList $ map fst maze)
  where
    tiles = [ ((x,y),t) | (y,xs) <- zip [0..] (lines raw), (x,t) <- zip [0..] xs ]
    [entrance] = [ p | (p,'@') <- maze ]
    keys  = M.fromList [ (c,p) | (p,c) <- filter (isLower . snd) maze ]
    doors = M.fromList [ (p,c) | (p,c) <- filter (isUpper . snd) maze ]
    maze  = [ (p,t) | (p,t) <- tiles, t /= '#']

-- | give each *reachable* tile a distance from a given coordinate
-- reachability is determined by the given `f_keys`
flood :: Maze -> [Char] -> Coord -> Flood
flood m@Maze{..} gotten start = dists
  where
    dists = go (M.singleton start 0) 0 [start]

    go done d ps
      | null expand = done
      | otherwise = go done' (d+1) expand
      where
        candidates = [ q | p <- ps, q <- adj m p ]

        expand = [ q | q <- candidates
                     , q `M.notMember` done
                     , {- traceShow ("expand",q,toLower <$> (doors M.!? q), gotten) $ -}
                       q `M.notMember` doors || toLower (doors M.! q) `elem` gotten ]

        done' = M.union done (M.fromList [ (q,d+1) | q <- expand ])
        -- remaining' = remaining `S.difference` (M.keysSet done')


adj :: Maze -> Coord -> [Coord]
adj Maze{..} (x,y) = filter (`S.member` maze) cardinals
  where
    cardinals = [(x,y-1),(x+1,y),(x,y+1),(x-1,y)]

part1 m@Maze{..} = solve start 0 [] goal
  where
    start = entrance
    goal :: [Char]
    goal = M.keys keys

    solve p d gotten missing
      | d > 4248     = []
      | null missing = [d]
      | otherwise    = (if not (null solutions)
                        then traceShow ("solutions",solutions,"gotten",gotten,"missing",missing)
                        else id) solutions
        where
          dists = flood m gotten p

          reachable = [ (k,d') | k <- missing, Just d' <- pure (dists M.!? (keys M.! k)) ]

          solutions = concat
            [ solve (keys M.! k) (d+d') got' mis'
             | (k,d') <- reachable
             , let got' = k : gotten
             , let mis' = L.delete k missing ]
