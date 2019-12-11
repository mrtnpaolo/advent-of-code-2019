module Main (main) where

import Advent

import Data.Ord
import Data.Bool
import Data.List
import Data.Foldable

import Data.Set (Set)
import qualified Data.Set as S

import Control.Arrow ((***))

main :: IO ()
main =
  do asteroids <- S.fromList . parse <$> getRawInput 10
     printSpace asteroids
     let (base,count) = part1 asteroids
     print base
     print count
     -- print (((part2 base asteroids) !!) <$> map pred [1,2,3,10,20,50,100,199,200,201,299])
     print . (\(x,y) -> x*100 + y) $ part2 base asteroids !! pred 200
  where
    parse raw = [ (x,y) | (y,ys) <- zip [0..] (lines raw), (x,'#') <- zip [0..] ys ]

type Coord = (Int,Int)
type Space = Set Coord

part1 :: Space -> (Coord,Int)
part1 coords = maximumBy (comparing snd) [ (p,visible coords p) | p <- toList coords ]

part2 :: Coord -> Space -> [Coord]
part2 center coords = map (add center) $ go relative
  where
    relative = S.map (sub center) (S.delete center coords)

    -- go :: Space -> [(Coord,Double)]
    go asteroids
      | S.null asteroids = []
      | otherwise        = vaporized ++ go asteroids'
      where
        asteroids' = S.difference asteroids (S.fromList vaporized)
        vaporized = sortOn angle [ p | p <- toList asteroids, visibleFromOrigin asteroids p ]
        angle :: Coord -> Double
        angle (x,y) = rotate $ atan2 (fromIntegral y) (fromIntegral x)
          where
            rotate :: Double -> Double
            rotate a = let b = a + pi/2
                           k = fromIntegral (floor $ b / (2*pi))
                       in b - 2*pi * k

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

add :: Coord -> Coord -> Coord
add (x,y) (u,v) = (x+u,y+v)

sub :: Coord -> Coord -> Coord
sub (x,y) (u,v) = (u-x,v-y)

printSpace :: Space -> IO ()
printSpace coords =
  for_ [0..yM] $ \y ->
    do for_ [0..xM] $ \x ->
         putChar $ bool '.' '#' $ (x,y) `elem` coords
       putChar '\n'
  where
    (xM,yM) = maximum *** maximum $ unzip $ toList coords
