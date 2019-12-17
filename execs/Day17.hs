{-# OPTIONS_GHC -Wno-type-defaults -Wno-missing-signatures -Wno-unused-matches -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Advent
import Advent.IntCode
import Data.Char
import Data.Foldable
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Control.Monad

import Debug.Trace

main :: IO ()
main =
  do mem <- map (read::String->Int) . words . map sep <$> getRawInput 17
     let space = map chr (run mem [])
     -- putStrLn (clean space)
     let m = M.fromList [ ((x,y),c) | (y,xs) <- zip [0..] (lines space), (x,c) <- zip [0..] xs ]
     -- print m
     -- print (part1 m)
     -- print (search m)
     part2 (2 : tail mem)
  where
    sep ',' = ' '; sep x = x
    clean = map (\case { '.' -> ' '; x -> x })

part1 = sum . map (uncurry (*)) . intersections

intersections m = [ (x,y) | ((x,y),'#') <- M.toList m, f (x,y) ]
  where
    f (x,y) = all ('#'==) [ tile | p <- adj (x,y), Just tile <- pure (m M.!? p) ]

adj (x,y) = [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]

type Coord = (Int,Int)
data Move = L | R | F Int deriving (Show)

-- 3 routines: A, B, C
-- movement functions: L, R, <n>

part2 mem =
  do m1 <- setup inp (run' mem)
     xs <- video m1
     play xs
  where

    setup []     eff            = pure eff
    setup (x:xs) (Input f)      = putChar (chr x) >> setup xs (f x)
    setup xs     (Output n eff) = putChar (chr n) >> setup xs eff

    video (Stop) = pure []
    video (Input f) = error "asking for input during video?"
    video (Output n eff) = do ns <- video eff
                              pure (n:ns)

    play = mapM_ (putChar . chr)

data Dir = N | E | S | W deriving (Show)

turn L N = W
turn L E = N
turn L S = E
turn L W = S

turn R N = E
turn R E = S
turn R S = W
turn R W = N

-- search :: M.Map Coord Char -> Coord -> [Coord]
search m = go (N,start)
  where
    [start] = [ p | (p,'^') <- M.assocs m ]
    [end] = [ p
             | (p,'#') <- M.assocs m
             , length [ q | q <- adj p, Just '.' <- pure (M.lookup q m)] == 3 ]

    coords = [ p | (p,'#') <- M.assocs m ]

    turns = [L,R]
    forward = map F [12,8,6]

    candidates = [ [t,s] | t <- turns, s <- forward ]

    go (dir,src)
      | src == end = [] -- Just []
      | otherwise  = culled
      where
        culled = take 1 [ ([L,s],dst) | s <- forward, Just dst <- pure (moveBy (src,dir) [L,s]) ]
                ++
                 take 1 [ ([R,s],dst) | s <- forward, Just dst <- pure (moveBy (src,dir) [R,s]) ]
        -- culled = [ (steps,dest)
        --           | steps <- candidates
        --           , Just dest <- pure ((dir,src) `moveBy` steps) ]

        -- [ | (steps,x) <- culled, paths <- go x ]

    moveBy :: (Coord,Dir) -> [Move] -> Maybe (Coord,Dir)
    moveBy = foldM f
      where
        f (    p,d) L     = Just (p,turn L d)
        f (    p,d) R     = Just (p,turn R d)
        f ((x,y),d) (F n) = m M.!? dest >>= \_ -> pure (dest,d)
          where
            dest | N <- d = (x,y-n)
                 | E <- d = (x+n,y)
                 | S <- d = (x,y+n)
                 | W <- d = (x-n,y)

    -- test = let Just (lastPos,lastDir) = (start,N) `moveBy` byhand in lastPos == end

-- main' = "AB"
-- a = [L,F 12,R,F 6,R,F 10,R,F 12,R]
-- b = [F 8]
-- c = [L]

-- [F 12,R,F 8,L,F 6,R,F 8,L,F 12,R,F 6]

a = [ L, F 12, R, F  8, L, F  6, R, F  8, L, F  6 ]
b = [ R, F  8, L, F 12, L, F 12, R, F  8 ]
c = [ L, F  6, R, F  6, L, F 12 ]

main' = "ABAABCBCCB"

inp = concat
  [ encodeMain main'
  , encodeFun a
  , encodeFun b
  , encodeFun c
  , map ord "n\n"
  ]

-- | encode "ABC" -> "A,B,C\n" -> [65,44,66,44,67,10]
encodeMain = map ord . (++"\n") . L.intersperse ','

encodeMove L     = [ord 'L']
encodeMove R     = [ord 'R']
encodeMove (F n) = map ord (show n)

encodeFun = (++ (pure (ord '\n'))) . concat . L.intersperse [ord ','] . map encodeMove
