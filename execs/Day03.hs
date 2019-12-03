module Main (main) where

import Advent

import Data.Function
import Data.Foldable
import Control.Arrow ((&&&))

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Move = (Int -> Int,Int -> Int)
type Trace = [Move]

type Point = (Int,Int)

type Wire = Set Point
type Signal = Map Point Int

main :: IO ()
main =
  do [t1,t2] <- map parseTrace . lines <$> getRawInput 3
     print $ (part1 `on` toWire) t1 t2
     print $ (part2 `on` toSignal) t1 t2

  where

    parseTrace :: String -> Trace
    parseTrace = concat . map fromString . words . map sep
      where
        sep ',' = ' '
        sep  x  =  x

        fromString :: String -> Trace
        fromString ('U':n) = read n `replicate` (id,succ)
        fromString ('R':n) = read n `replicate` (succ,id)
        fromString ('D':n) = read n `replicate` (id,pred)
        fromString ('L':n) = read n `replicate` (pred,id)
        fromString _       = undefined

    toPoints :: Trace -> [Point]
    toPoints = scanl (\(x,y) (f,g) -> (f x,g y)) (0,0)

    toWire :: Trace -> Wire
    toWire = S.fromList . toPoints

    toSignal :: Trace -> Signal
    toSignal t = M.fromSet lowest (S.fromList points)
      where
        points = toPoints t
        signal = zip points [0..]
        lowest coord = minimum [ s | (c,s) <- signal, c == coord ]

part1 :: Wire -> Wire -> Int
part1 w1 w2 = minManhattan $ w1 `S.intersection` w2

minManhattan :: Set Point -> Int
minManhattan = S.findMin . S.deleteMin . S.map (uncurry ((+) `on` abs))

part2 :: Signal -> Signal -> Int
part2 s1 s2 = minimum . M.elems $ s
  where
    intersectionCoords = S.delete (0,0) $ (S.intersection `on` M.keysSet) s1 s2
    s1' = s1 `M.restrictKeys` intersectionCoords
    s2' = s2 `M.restrictKeys` intersectionCoords
    s = M.unionWith (+) s1' s2'

dumpWires :: Wire -> Wire -> IO ()
dumpWires w1 w2 =
  do forM_ [ym..yM] $ \y ->
       do forM_ [xm..xM] $ \x ->
            do putStr (at (x,y))
          putStr "\n"
  where
    (xm1, xM1) = (minimum &&& maximum) (fst `S.map` w1)
    (ym1, yM1) = (minimum &&& maximum) (snd `S.map` w1)
    (xm2, xM2) = (minimum &&& maximum) (fst `S.map` w2)
    (ym2, yM2) = (minimum &&& maximum) (snd `S.map` w2)
    (xm,xM) = (min xm1 xm2,max xM1 xM2)
    (ym,yM) = (min ym1 ym2,max yM1 yM2)

    at (0,0) = "O"
    at coord | coord `S.member` w1 && coord `S.member` w2 = "X"
             | coord `S.member` w1 = "1"
             | coord `S.member` w2 = "2"
             | otherwise           = "."
