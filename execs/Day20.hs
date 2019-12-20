{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Advent
import Advent.Search
import Data.Char
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Control.Arrow ((***), (&&&))
import Text.Printf

main :: IO ()
main =
  do putStrLn "Day20"
     -- m <- parse <$> readFile "inputs/input20-test01-23"
     m <- parse <$> readFile "inputs/input20-test02-58"
     -- m <- parse <$> getRawInput 20
     putStr (showMaze m)
     print (portals m)
     printf "raw map is %d tiles\ncorridors run a length of %d\n" (length (mazeChars m)) (length (corridors m))

cardinals :: Coord -> [Coord]
cardinals (x,y) = [(x,y-1),(x+1,y),(x,y+1),(x-1,y)]

type Coord = (Int,Int)
type Portal = String

data Dir = N | E | S | W deriving (Show)

data Maze = Maze
  { mazeChars :: M.Map Coord Char
  , w :: Int
  , h :: Int
  , portals     :: M.Map Coord Portal
  , portalsLocs :: M.Map Portal Coord
  , corridors :: S.Set Coord
  } deriving (Show)

parse :: String -> Maze
parse raw = Maze { mazeChars = mazeChars, w = w, h = h
                 , portals = portals
                 , portalsLocs = M.empty
                 , corridors = corridors }
  where
    mazeChars = M.fromList [ ((x,y),c) | (y,xs) <- zip [-2,-1..] (lines raw)
                                       , (x,c) <- zip [-2,-1..] xs ]
    rows = length (lines raw)
    cols = length (head (lines raw))
    h = rows - 4 -- maze's height
    w = cols - 4 -- maze's width

    hole = bfs next (w `div` 2,h `div` 2)
      where
        next p = [ q | q <- cardinals p
                     , let c = mazeChars M.! q
                     , isSpace c || isAlpha c ]
    ((hxm,hxM),(hym,hyM)) = minMax *** minMax $ unzip hole

    insidePortals = concat $
      [ [ (p,name) | p <- ns, '.' ==  mazeChars M.! p, let name = findName S p ]
      , [ (p,name) | p <- es, '.' ==  mazeChars M.! p, let name = findName W p ]
      , [ (p,name) | p <- ss, '.' ==  mazeChars M.! p, let name = findName N p ]
      , [ (p,name) | p <- ws, '.' ==  mazeChars M.! p, let name = findName E p ] ]
      where
        ns = [ (x,y) | x <- [hxm..hxM], let y = hym-1 ]
        ss = [ (x,y) | x <- [hxm..hxM], let y = hyM+1 ]
        ws = [ (x,y) | let x = hxm-1, y <- [hym..hyM] ]
        es = [ (x,y) | let x = hxM-1, y <- [hym..hyM] ]

    findName N (x,y) = map (mazeChars M.!) [ (x,y-2), (x,y-1) ]
    findName E (x,y) = map (mazeChars M.!) [ (x+1,y), (x+2,y) ]
    findName S (x,y) = map (mazeChars M.!) [ (x,y+1), (x,y+2) ]
    findName W (x,y) = map (mazeChars M.!) [ (x-2,y), (x-1,y) ]

    portals = M.fromList insidePortals

    corridors = S.fromList [ p | (p,'.') <- M.assocs mazeChars ]

showMaze :: Maze -> String
showMaze Maze{..} = unlines $
  (flip map) [-2..h+1] $ \y ->
    (flip map) [-2..w+1] $ \x ->
      mazeChars M.! (x,y)

minMax :: [Int] -> (Int, Int)
minMax = L.minimum &&& L.maximum
