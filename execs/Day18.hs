{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-top-binds -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns, TypeSynonymInstances, FlexibleInstances #-}
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
     print (flood maze (entrance maze,keys maze))
     -- print (next maze (entrance maze,keys maze))
     -- print `mapM_` do takeWhile (not.null) $ iterate (concatMap (next maze)) [(entrance maze,keys maze)]
     -- let solutions = part1 maze
     -- print solutions
     -- print (minimum solutions)

type Coord = (Int,Int)
type Tile = Char

newtype Keys = Keys { fromKeys :: M.Map Coord Char }

instance Show Keys where
  show = L.sort . M.elems . fromKeys

data Maze = Maze
  { entrance :: Coord
  , keys     :: Keys
  , doors    :: M.Map Coord Char
  , maze     :: S.Set Coord
  } -- deriving ()

parse :: String -> Maze
parse raw = Maze entrance (Keys keys) doors (S.fromList $ map fst maze)
  where
    tiles = [ ((x,y),t) | (y,xs) <- zip [0..] (lines raw), (x,t) <- zip [0..] xs ]
    [entrance] = [ p | (p,'@') <- maze ]
    keys  = M.fromList [ (p,c) | (p,c) <- filter (isLower . snd) maze ]
    doors = M.fromList [ (p,c) | (p,c) <- filter (isUpper . snd) maze ]
    maze  = [ (p,t) | (p,t) <- tiles, t /= '#']

type Point = (Coord,Keys) -- (position, missing keys)

flood :: Maze -> Point -> [(Point,Int)]
flood m@Maze{..} (start,startMissing) = go (S.singleton start) [start] 0
  where
    go seen frontier d
      | null frontier = pts
      | otherwise     = pts ++ go seen' frontier' (d+1)
      where
        candidates = [ q | p <- frontier, q <- adj m p
                         , q `S.notMember` seen
                         , q `M.notMember` doors || toLower (doors M.! q) `notElem` M.elems (fromKeys startMissing) ]
        (found,free) = L.partition (`M.member` (fromKeys startMissing)) candidates
        seen' = S.union seen (S.fromList candidates)
        frontier' = free
        pts = [ ((p,Keys $ M.delete p (fromKeys startMissing)),d+1) | p <- found ]


adj :: Maze -> Coord -> [Coord]
adj Maze{..} (x,y) = filter (`S.member` maze) cardinals
  where
    cardinals = [(x,y-1),(x+1,y),(x,y+1),(x-1,y)]
