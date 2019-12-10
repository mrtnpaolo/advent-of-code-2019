module Main (main) where

import Advent

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M

import Data.Char (intToDigit)
import Data.Maybe (fromMaybe)
import Data.Traversable (forM)

import Text.Printf

main :: IO ()
main =
  do space <- parse <$> getRawInput 10
     print space
     let (xM,yM) = (maximum (S.map fst space), maximum (S.map snd space))
     print xM
     print yM
     -- mapM_ print (centeredRays (2,2) (xM,yM))
     let s' = part1 space
     mapM_ print $ S.elems $ s'
     putStr . showVis (xM,yM) . M.fromList . S.elems $ s'

type Coord = (Int,Int)
type Space = Set Coord
type Visibility = Map Coord Int

part1 space = S.map visibleFrom space
  where
    (xM,yM) = (maximum (S.map fst space), maximum (S.map snd space))

    visibleFrom (cx,cy) = (\xs -> ((cx,cy),length xs)) . filter (any asteroid) $ centeredRays (cx,cy) (xM,yM)

    asteroid = (`S.member` space)

showVis :: (Int,Int) -> Visibility -> String
showVis (xM,yM) vs = unlines $
  (flip map) [0..yM] $ \y ->
    concat $ (flip map) [0..xM] $ \x ->
      -- fromMaybe '.' (intToDigit <$> vs !? (x,y))
      fromMaybe "  ." (printf " %2d" <$> vs !? (x,y))

parse :: String -> Space
parse raw = S.fromList [ (x,y) | (y,ys) <- space, (x,v) <- ys, v == '#' ]
  where
    space = zip [0..] (map (zip [0..]) (lines raw))

-- | Finite rays in all directions clockwise from the positive X axis around (cx,cy)
centeredRays :: (Int,Int) -> (Int,Int) -> [[Coord]]
centeredRays (cx,cy) (xM,yM) = filter (not . null) . map (takeWhile inside . map (translate (cx,cy))) $ rays (xM,yM)
  where
    translate (cx,cy) (x,y) = (cx+x,cy+y)
    inside (x,y) = 0 <= x && x <= xM && 0 <= y && y <= yM

-- | Lazy infinite rays in all directions clockwise from the positive X axis around the origin
rays :: (Int {- ^ max x -}, Int {- ^ max y -}) -> [[Coord]]
rays (xM,yM)
  | xM < 2 || yM < 2 = error $ "can't cast rays in a " ++ show xM ++ "x" ++ show yM ++ " space"
  | otherwise      = concat [ [x14], q4, [y34], q3, [x23], q2, [y12], q1 ]
  where
    -- ray in the second quadrant
    ray (x,y) = takeWhile (\(x,y) -> x<xM&&y<yM) $ scanl (\(a,b)(c,d)->(a+c,b+d)) (x,y) (repeat (x,y))
    q4 = [ ray (x,y) | y <- [1,2..yM], x <- [xM,xM-1..1] ]
    q3 = map (map (\(x,y) -> (-x,y))) q4
    q2 = map (map (\(x,y) -> (x,-y))) q3
    q1 = map (map (\(x,y) -> (x,-y))) q2
    x14 = [ ( x,  0) | x <- [1..] ]
    y34 = [ ( 0,  y) | y <- [1..] ]
    x23 = [ (-x,  0) | x <- [1..] ]
    y12 = [ ( 0, -y) | y <- [1..] ]

    -- filterRecasts = go S.empty
    --   where
    --     go _ [] = []
    --     go seen (r:rs)
    --       | any (`S.member` seen) r =     go seen                            rs
    --       | otherwise               = r : go (seen `S.union` (S.fromList r)) rs
