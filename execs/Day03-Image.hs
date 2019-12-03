-- warning! this is VERY slow!

module Main (main) where

import Advent

import Data.Bifunctor
-- import Data.Function
-- import Data.Foldable
import Control.Arrow ((&&&))

-- import Data.Map (Map)
-- import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Text.Printf

import Codec.Picture

type Move = (Int -> Int,Int -> Int)
type Trace = [Move]

type Point = (Int,Int)

type Wire = Set Point

main :: IO ()
main =
  do [w1,w2] <- map (S.fromList . toPoints . parseTrace) . lines <$> getRawInput 3
     let ((w,h),img) = toImage w1 w2
     printf "image: %dx%d pixels\n" w h
     let raw = generateImage img w h
     let output = "wires.png"
     printf "writing out %s..." output
     writePng output raw
     printf "done\n"
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

toImage :: Wire -> Wire -> ((Int,Int),(Int -> Int -> PixelRGB8))
toImage w1 w2 = ((w,h),image)
  where
    ((xm1, xM1),(ym1, yM1)) = (minimum &&& maximum) w1
    ((xm2, xM2),(ym2, yM2)) = (minimum &&& maximum) w2
    (xm,xM) = (min xm1 xm2,max xM1 xM2)
    (ym,yM) = (min ym1 ym2,max yM1 yM2)

    (w,h) = (xM-xm,yM-ym)

    at (0,0) = "O"
    at coord | coord `S.member` w1 && coord `S.member` w2 = "X"
             | coord `S.member` w1 = "1"
             | coord `S.member` w2 = "2"
             | otherwise           = "."

    image x y =
      case at (x+xm,y+ym) of
        "O" -> PixelRGB8 255 255 255
        "X" -> PixelRGB8 255 255   0
        "1" -> PixelRGB8 255   0   0
        "2" -> PixelRGB8   0 255   0
        _   ->
          if any ("X" ==) (at <$> around (x+xm,y+ym))
          then PixelRGB8 255 255   0
          else PixelRGB8   0   0   0
      where
        around (x,y) = [(x,y+1),(x+1,y+1),(x+1,y),(x+1,y-1),(x,y-1),(x-1,y-1),(x-1,y),(x-1,y+1)]
