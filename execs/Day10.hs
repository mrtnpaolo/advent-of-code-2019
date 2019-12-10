module Main (main) where

import Advent

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M

import Data.Ord (comparing)
import Data.Bool (bool)
import Data.Char (intToDigit)
import Data.List (nub, (\\), maximumBy)
import Data.Maybe (fromMaybe)
import Data.Bifunctor
import Data.Traversable (for)
import Data.Foldable (forM_)

import Text.Printf
import Debug.Trace

main :: IO ()
main =
  do space <- parse <$> getRawInput 10
     print space
     let (xM,yM) = (maximum (S.map fst space), maximum (S.map snd space))
     print xM
     print yM

     putStrLn "=== part 1"
     let detections = part1 space
     putStrLn $ showVis (size space) detections
     print $ maximumBy (comparing snd) $ M.assocs detections

type Coord = (Int,Int)
type Space = Set Coord

size space = (maximum (S.map fst space), maximum (S.map snd space))

part1 :: Set Coord -> Map Coord Int
part1 space = x
  where
    x = M.fromList [ (p,detect p space) | p <- S.elems space ]

detect :: Coord -> Set Coord -> Int
detect p space = sweep 0 (S.delete p space) (tail $ map (translate p) spiral)
  where
    sweep :: Int -> Set Coord -> [Coord] -> Int
    sweep n space (q:qs)

      | S.null space       = n

      -- asteroid is detected
      | q `S.member` space =
        let rs = S.fromList $ takeWhile inside (ray p q)
            dbg = if (22,26) == p
                    then trace (printf "target: (%d,%d) %s" (fst p) (snd p) (show rs) :: String) 
                    else id
        in dbg $ sweep (succ n) (space `S.difference` rs) qs

      | otherwise          = sweep n space qs

    inside (x,y) = 0 <= x && x <= xM && 0 <= y && y <= yM

    (xM,yM) = size space

translate (x,y) (dx,dy) = (x+dx,y+dy)

ray :: Coord -> Coord -> [Coord]
ray (x,y) (x',y') = tail $ iterate (\(x,y)->(x+dx,y+dy)) (x,y)
  where
    (dx,dy)
      -- 45 deg
      | abs (x'-x) == abs (y'-y) = (signum $ x'-x,signum $ y'-y) 
      -- horizontal & vertical
      | x' == x = (0,signum $ y'-y)
      | y' == y = (signum $ x'-x,0)
      -- crooked
      | otherwise = (x'-x,y'-y)

-- counter-clockwise spiral
spiral :: [Coord]
spiral = scanl go (0,0) $ concat [ replicate ((i `div` 2) + 1) $ off (i `mod` 4) | i <- [0..] ]
  where
    off 0 = bimap succ id
    off 1 = bimap id pred
    off 2 = bimap pred id
    off 3 = bimap id succ
    go (x,y) f = f (x,y)

showRay (xM,yM) center ray = unlines $
  (flip map) [0..yM] $ \y ->
    concat $ (flip map) [0..xM] $ \x ->
      if (x,y) == center
        then "  O"
        else bool "  ." "  R" ((x,y) `S.member` ray)

showVis (xM,yM) counts = unlines $
  (flip map) [0..yM] $ \y ->
    concat $ (flip map) [0..xM] $ \x ->
      fromMaybe "   ." (printf " %3d" <$> counts !? (x,y))

parse :: String -> Space
parse raw = S.fromList [ (x,y) | (y,ys) <- space, (x,v) <- ys, v == '#' ]
  where
    space = zip [0..] (map (zip [0..]) (lines raw))
