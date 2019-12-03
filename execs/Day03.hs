module Main (main) where

import Advent

import Data.Function
import Data.Bifunctor
import Data.Foldable
import Control.Arrow ((&&&))

-- import Data.IntMap.Strict (IntMap, (!))
-- import qualified Data.IntMap.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

type Move = (Int -> Int,Int -> Int)
type Trace = [Move]
type Point = (Int,Int)
type Wire = Set Point

main :: IO ()
main =
  do [w1,w2] <- map (fromTrace . concat) . map parse . lines <$> getRawInput 3
     -- dumpWires w1 w2
     print $ part1 w1 w2
  where
    parse = map fromString . words . map sep
    sep ',' = ' '
    sep  x  =  x
    fromString :: String -> Trace
    fromString ('U':n) = read n `replicate` (id,succ)
    fromString ('R':n) = read n `replicate` (succ,id)
    fromString ('D':n) = read n `replicate` (id,pred)
    fromString ('L':n) = read n `replicate` (pred,id)

part1 :: Wire -> Wire -> Int
part1 w1 w2 = minManhattan $ w1 `S.intersection` w2

minManhattan :: Set Point -> Int
minManhattan = S.findMin . S.deleteMin . S.map (uncurry ((+) `on` abs))

fromTrace :: Trace -> Wire
fromTrace = S.fromList . scanl app (0,0)
  where
    app (x,y) (f,g) = (f x, g y)

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