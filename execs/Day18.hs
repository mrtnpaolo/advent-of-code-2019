{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-top-binds -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns, TypeSynonymInstances, FlexibleInstances #-}
module Main (main) where

import Advent
import Data.Char
import Data.Maybe
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Graph.Inductive

import Debug.Trace

main :: IO ()
main =
  do m <- parse <$> getRawInput 18 -- readFile "inputs/input18-test02" -- getRawInput 18
     let sol = findOneSolution m (entrance m,keys m)
     print `mapM_` sol
     print (sum (map snd sol))

findOneSolution :: Maze -> Point -> [(Point,Int)]
findOneSolution m (pos,miss) =
  case next of
    (_,[]) -> [step]
    _      -> step : findOneSolution m next
  where
    step@(next,_) = pick (flood m (pos,miss))
    pick pts =
      case (filter (\((c,_),_) -> (keyLocs m M.! c) `elem` miss) pts) of
        [x] -> x
        [_,x] -> x
        (x:_) -> x

type Coord = (Int,Int)
type Tile = Char

type Keys = [Char]

data Maze = Maze
  { entrance :: Coord
  , keys     :: Keys
  , keyLocs  :: M.Map Coord Char
  , doors    :: M.Map Coord Char
  , maze     :: S.Set Coord
  } deriving (Show)

parse :: String -> Maze
parse raw = Maze entrance keys keyLocs doors (S.fromList $ map fst maze)
  where
    tiles = [ ((x,y),t) | (y,xs) <- zip [0..] (lines raw), (x,t) <- zip [0..] xs ]
    [entrance] = [ p | (p,'@') <- maze ]
    keyLocs = M.fromList [ (p,c) | (p,c) <- filter (isLower . snd) maze ]
    doors   = M.fromList [ (p,c) | (p,c) <- filter (isUpper . snd) maze ]
    keys    = M.elems keyLocs
    maze    = [ (p,t) | (p,t) <- tiles, t /= '#']

type Point = (Coord,Keys) -- (position, missing keys)

flood :: Maze -> Point -> [(Point,Int)]
flood m@Maze{..} (start,startMissing) = go (S.singleton start) [start] 0
  where
    go seen frontier d
      | null frontier = []
      | otherwise     = pts ++ go seen' frontier' (d+1)
      where
        candidates = [ q | q <- (S.toList $ S.difference (S.fromList [ q | p <- frontier, q <- adj m p ]) seen)
                         , q `M.notMember` doors || toLower (doors M.! q) `notElem` startMissing ]
        (found,free) = L.partition (`M.member` (M.filter (\k -> k `elem` startMissing) keyLocs)) candidates
        seen' = S.union seen (S.fromList candidates)
        frontier' = free
        pts = [ ((p,L.delete (keyLocs M.! p) startMissing),d+1) | p <- found ]


adj :: Maze -> Coord -> [Coord]
adj Maze{..} (x,y) = filter (`S.member` maze) cardinals
  where
    cardinals = [(x,y-1),(x+1,y),(x,y+1),(x-1,y)]
