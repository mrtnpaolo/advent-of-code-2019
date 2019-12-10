module Main (main) where

import Advent

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M

import Data.Bool (bool)
import Data.Char (intToDigit)
import Data.List (nub, (\\))
import Data.Maybe (fromMaybe)
import Data.Bifunctor (second)
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

     let test02 = rays space (0,2)
     putStrLn "=== rays from (0,2)"
     forM_ test02 $ \cs ->
       putStrLn $ showRay (size space) (0,2) (S.fromList cs)

     -- putStrLn "=== rays from (2,2)"
     -- mapM_ print (rays space (2,2))
     putStrLn "=== part 1"
     mapM_ print (part1 space)
     putStrLn $ showVis (size space) (part1 space)

type Coord = (Int,Int)
type Space = Set Coord

size space = (maximum (S.map fst space), maximum (S.map snd space))

part1 space = illuminate space
  where
    illuminate = S.map (\p -> (p,count $ rays space p))
    count = length . filter (any (`S.member` space))

showRay (xM,yM) center ray = unlines $
  (flip map) [0..yM] $ \y ->
    concat $ (flip map) [0..xM] $ \x ->
      if (x,y) == center
        then "  O"
        else bool "  ." "  R" ((x,y) `S.member` ray)

showVis (xM,yM) v = unlines $
  (flip map) [0..yM] $ \y ->
    concat $ (flip map) [0..xM] $ \x ->
      fromMaybe "  ." (printf " %2d" <$> vs !? (x,y))
  where
    vs = M.fromList (S.elems v)

parse :: String -> Space
parse raw = S.fromList [ (x,y) | (y,ys) <- space, (x,v) <- ys, v == '#' ]
  where
    space = zip [0..] (map (zip [0..]) (lines raw))

rays :: Space -> (Int,Int) -> [[Coord]]
rays space (cx,cy) = traceShowId $ map ray sweep
  where
    (xM,yM) = size space

    translate (x,y) (dx,dy) = (x+dx,y+dy)

    inside (x,y) = 0 <= x && x <= xM && 0 <= y && y <= yM

    ray (dx,dy) = takeWhile inside $ tail $ iterate (\(x,y)->(x+dx,y+dy)) (cx,cy)

    --      x               xM
    --   y  . . . \ . / . . .
    --      . . . \ . / . . .
    --      \ \ \ \ | / / / /
    --      . . . - C - . . .
    --      / / / / | \ \ \ \
    --      . . . / . \ . . .
    --   yM . . . / . \ . . .
    --
    --        x 1 2 3 4 5 6 7 xM=8
    --                F   G
    --   y    . . . .-4 .-4 . .
    --   1    . . . .-3 .-3 . .
    --   2    . . . .-2 .-2 . .
    --   3 E -5-4-3-2 \ | / 2 3 H
    --   4    . . . . - C - . .      C = (5,4)
    --   5 D -5-4-3-2 / | \ 2 3 A
    --   6    . . . . 2 . 2 . .
    --   7    . . . . 3 . 3 . .
    -- 8=yM   . . . . 4 . 4 . .
    --                C   B
    sweep = axes ++ diags ++ crooked
      where
        axes = [ (1,0), (0,1), (-1,0), (0,-1) ]
        diags = [ (1,1), (-1,1), (-1,-1), (1,-1) ]
        crooked = concat $ takeWhile (inside . translate (cx,cy)) <$>
          [ [ ( dx,  1) | dx <- [2,3..] ] -- A
          , [ (  1, dy) | dy <- [2,3..] ] -- B
          , [ ( -1, dy) | dy <- [2,3..] ] -- C
          , [ (-dx,  1) | dx <- [2,3..] ] -- D
          , [ (-dx, -1) | dx <- [2,3..] ] -- E
          , [ ( -1,-dy) | dy <- [2,3..] ] -- F
          , [ (  1,-dy) | dy <- [2,3..] ] -- G
          , [ ( dx, -1) | dx <- [2,3..] ] -- H
          ]
