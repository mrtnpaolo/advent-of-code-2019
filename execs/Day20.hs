{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Advent
import Advent.Search
import Data.Char
import Data.Maybe
import Data.Bifunctor
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Control.Arrow ((***), (&&&))
import Control.Applicative
import Text.Printf

main :: IO ()
main =
  do putStrLn "Day20"
     -- m <- parse <$> readFile "inputs/input20-test01-23"
     -- m <- parse <$> readFile "inputs/input20-test02-58"
     m <- parse <$> getRawInput 20
     putStr (showMaze m)
     printf "insidePortals: %s\n" $ show (insidePortals m)
     printf "outsidePortals: %s\n" $ show (outsidePortals m)
     printf "raw map is %d tiles\ncorridors run a length of %d\n" (length (mazeChars m)) (length (corridors m))
     let entrance = outsidePortalsLoc m M.! "AA"
     -- printf "reachable from AA %s: %s\n" (show entrance) (show $ map (bimap (portalName m) id) $ reachablePortals m entrance)

     print $ head $ filter (("ZZ" ==) . fst) $ map (bimap (portalName m) id) $ shortest m

portalName :: Maze -> Coord -> String
portalName Maze{..} p
  | Just name <- outsidePortals M.!? p = name
  | Just name <- insidePortals  M.!? p = map toLower name
  | otherwise = error "not a portal"

type Coord = (Int,Int)
type Portal = String

data Dir = N | E | S | W deriving (Show)

data Maze = Maze
  { mazeChars :: M.Map Coord Char
  , w :: Int
  , h :: Int
  , insidePortals     :: M.Map Coord Portal
  , outsidePortals    :: M.Map Coord Portal
  , insidePortalsLoc  :: M.Map Portal Coord
  , outsidePortalsLoc :: M.Map Portal Coord
  , corridors :: S.Set Coord
  } deriving (Show)

-- Graph

data Side = Out | In deriving (Show, Eq, Ord)

shortest :: Maze -> [(Coord,Int)]
shortest m@Maze{..} = tail $ bfsOn repr next begin
  where
    entrance = outsidePortalsLoc M.! "AA"
    begin = (entrance,0)
    repr (p,d) = (p,d)
    next (p,d) = catMaybes
      [ if outsidePortalsLoc M.! "ZZ" == q
        then Just (q,d+d')
        else (\a -> (a,d+1+d')) <$> teleport m q
      | (q,d') <- reachablePortals m p ]

teleport :: Maze -> Coord -> Maybe Coord
teleport Maze{..} p
  | Just name <- outsidePortals M.!? p = insidePortalsLoc  M.!? name
  | Just name <- insidePortals  M.!? p = outsidePortalsLoc M.!? name
  | otherwise = error "not a portal"

reachablePortals :: Maze -> Coord -> [(Coord,Int)]
reachablePortals m@Maze{..} p
  | p `M.notMember` insidePortals && p `M.notMember` outsidePortals = error "non-portal coordinate"
  | otherwise = filter (isJust . isPortal m . fst) flood
  where
    begin = (p,0)
    repr = fst
    next (p,d) = [ (q,d+1) | q <- cardinals p, q `S.member` corridors ]
    flood = tail (bfsOn repr next begin) -- tail to not return the starting portal

isPortal :: Maze -> Coord -> Maybe String
isPortal Maze{..} p = isOutsidePortal <|> isInsidePortal
  where
    isOutsidePortal = outsidePortals M.!? p
    isInsidePortal  = insidePortals M.!? p

-- Parsing

parse :: String -> Maze
parse raw = Maze { mazeChars = mazeChars, w = w, h = h
                 , insidePortals  = M.fromList insidePortals
                 , outsidePortals = M.fromList outsidePortals
                 , insidePortalsLoc  = M.fromList [ (name,c) | (c,name) <- insidePortals ]
                 , outsidePortalsLoc = M.fromList [ (name,c) | (c,name) <- outsidePortals ]
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
        es = [ (x,y) | let x = hxM+1, y <- [hym..hyM] ]

    outsidePortals = concat $
      [ [ (p,name) | p <- ns, '.' ==  mazeChars M.! p, let name = findName N p ]
      , [ (p,name) | p <- es, '.' ==  mazeChars M.! p, let name = findName E p ]
      , [ (p,name) | p <- ss, '.' ==  mazeChars M.! p, let name = findName S p ]
      , [ (p,name) | p <- ws, '.' ==  mazeChars M.! p, let name = findName W p ] ]
      where
        ns = [ (x,y) | x <- [0..w-1], let y = 0     ]
        ss = [ (x,y) | x <- [0..w-1], let y = h-1   ]
        ws = [ (x,y) | let x = 0,     y <- [0..h-1] ]
        es = [ (x,y) | let x = w-1,   y <- [0..h-1] ]

    findName N (x,y) = map (mazeChars M.!) [ (x,y-2), (x,y-1) ]
    findName E (x,y) = map (mazeChars M.!) [ (x+1,y), (x+2,y) ]
    findName S (x,y) = map (mazeChars M.!) [ (x,y+1), (x,y+2) ]
    findName W (x,y) = map (mazeChars M.!) [ (x-2,y), (x-1,y) ]

    corridors = S.fromList [ p | (p,'.') <- M.assocs mazeChars ]

showMaze :: Maze -> String
showMaze Maze{..} = unlines $
  (flip map) [-2..h+1] $ \y ->
    (flip map) [-2..w+1] $ \x ->
      mazeChars M.! (x,y)

minMax :: [Int] -> (Int, Int)
minMax = L.minimum &&& L.maximum

cardinals :: Coord -> [Coord]
cardinals (x,y) = [(x,y-1),(x+1,y),(x,y+1),(x-1,y)]
