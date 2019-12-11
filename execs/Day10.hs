module Main (main) where

import Advent

import Data.Ord
import Data.Bool
import Data.Foldable

import Data.Set (Set)
import qualified Data.Set as S

import Control.Arrow ((***))

main :: IO ()
main =
  do asteroids <- S.fromList . parse <$> getRawInput 10
     printSpace asteroids
     print (part1 asteroids)
  where
    parse raw = [ (x,y) | (y,ys) <- zip [0..] (lines raw), (x,'#') <- zip [0..] ys ]

type Coord = (Int,Int)
type Space = Set Coord

part1 :: Space -> Int
part1 coords = snd $ maximumBy (comparing snd) [ (p,visible coords p) | p <- toList coords ]

visible :: Space -> Coord -> Int
visible coords center = count (visibleFromOrigin relative) (S.delete (0,0) relative)
  where
    relative = S.map (sub center) coords
    count predicate = foldl' (\n x -> if predicate x then succ n else n) 0

visibleFromOrigin :: Space -> Coord -> Bool
visibleFromOrigin coords (x,y) = no obstacle between
  where
    k = gcd x y
    u = x `div` k
    v = y `div` k

    no = (not .) . any
    obstacle = flip S.member coords
    between = [ (i * u,i * v) | i <- [1..k-1] ]

sub :: Coord -> Coord -> Coord
sub (x,y) (u,v) = (x-u,y-v)

printSpace :: Space -> IO ()
printSpace coords =
  for_ [0..yM] $ \y ->
    do for_ [0..xM] $ \x ->
         putChar $ bool '.' '#' $ (x,y) `elem` coords
       putChar '\n'
  where
    (xM,yM) = maximum *** maximum $ unzip $ toList coords
