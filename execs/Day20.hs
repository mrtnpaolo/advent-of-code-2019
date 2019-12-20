{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Advent
import Advent.Search
import Data.Char
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Control.Arrow ((***), (&&&))

main :: IO ()
main =
  do putStrLn "Day20"
     m <- parse <$> readFile "inputs/input20-test01-23"
     -- m <- parse <$> readFile "inputs/input20-test02-58"
     -- m <- parse <$> getRawInput 20
     putStr (showMaze m)
     print $ map (\p -> (p, mazeChars m M.! p)) (M.keys $ portals m)

cardinals :: Coord -> [Coord]
cardinals (x,y) = [(x,y-1),(x+1,y),(x,y+1),(x-1,y)]

type Coord = (Int,Int)
type Portal = String

data Maze = Maze
  { mazeChars :: M.Map Coord Char
  , w :: Int
  , h :: Int
  , portals     :: M.Map Coord Portal
  , portalsLocs :: M.Map Portal Coord
  } deriving (Show)

parse :: String -> Maze
parse raw = Maze { mazeChars = mazeChars, w = w, h = h
                 , portals = portals
                 , portalsLocs = M.empty }
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

    holeBorder :: S.Set Coord
    holeBorder = S.fromList . concat $
      [ [ (x,hym-1), (x,hyM+1) ] | x <- [hxm..hxM] ] ++
      [ [ (hxm-1,y), (hxM+1,y) ] | y <- [hym..hyM] ]

    insidePortals = filter (('.' ==) . (mazeChars M.!)) (S.toList holeBorder)

    portals = M.fromList [ (p,name) | p <- insidePortals, let name = "xxx" ]

showMaze :: Maze -> String
showMaze Maze{..} = unlines $
  (flip map) [-2..h+1] $ \y ->
    (flip map) [-2..w+1] $ \x ->
      mazeChars M.! (x,y)

minMax :: [Int] -> (Int, Int)
minMax = L.minimum &&& L.maximum
