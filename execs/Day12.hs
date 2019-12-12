{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports -Wno-missing-signatures -Wno-unused-top-binds #-}
module Main (main) where

import Advent
import Data.List
import Data.Foldable
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Arrow ((***))
-- import Debug.Trace

main :: IO ()
main = print t
  where
    (tx,ty,tz) = part2 moons
    t = lcm (lcm tx ty) tz

type P = (Int,Int,Int) -- position
type V = (Int,Int,Int) -- velocity
type Moon = (P,V)
type Moons = Map Int Moon

moons :: Moons
moons = M.fromList $ zip [0..]
  [ (( 4,  1, 1),(0,0,0))
  , ((11,-18,-1),(0,0,0))
  , ((-2,-10,-4),(0,0,0))
  , ((-7, -2,14),(0,0,0)) ]
-- moons :: Moons
-- moons = M.fromList $ zip [0..]
--   [ ((-1, 0,  2),(0,0,0))
--   , (( 2,-10,-7),(0,0,0))
--   , (( 4, -8, 8),(0,0,0))
--   , (( 3,  5,-1),(0,0,0)) ]

opposite (a,b,c) = (-a,-b,-c)

pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

gravity1 a b = case compare a b of LT -> -1; EQ -> 0; GT -> 1

gravity :: Moon -> Moon -> V
gravity ((x0,y0,z0),_) ((x1,y1,z1),_) =
  (gravity1 x0 x1, gravity1 y0 y1, gravity1 z0 z1)

effects :: Moons -> Map Int [V]
effects ms = M.unionsWith (++) $
  [ M.fromList [(m,[opposite v]),(n,[v])]
  | (m,n) <- pairs ids
  , let v = gravity (ms!m) (ms!n)
  ]
  where
    ids = M.keys ms

applyGravity :: Moons -> Moons
applyGravity ms = M.mapWithKey f ms
  where
    effs = effects ms

    f i m@(p,(vx,vy,vz)) = (p,(vx',vy',vz'))
      where
        vs = effs ! i
        vx' = sum (vx : map fst3 vs)
        vy' = sum (vy : map snd3 vs)
        vz' = sum (vz : map trd3 vs)

fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c

applyVelocity :: Moons -> Moons
applyVelocity = fmap f
  where
    f ((x,y,z),v@(vx,vy,vz)) = ((x+vx,y+vy,z+vz),v)

step :: Moons -> Moons
step = applyVelocity . applyGravity

energy :: Moons -> Int
energy = sum . map e . toList
  where
    e m = p m * k m

    p ((x,y,z),_) = abs x + abs y + abs z
    k (_,(vx,vy,vz)) = abs vx + abs vy + abs vz

-- ((x0,vx0),(x1,vx1),(x2,vx2),(x3,vx3)) -- and similar for y and z
type Mono = Set ((Int,Int),(Int,Int),(Int,Int),(Int,Int))

part2 ms = go 0 (S.empty,S.empty,S.empty) (-1,-1,-1) (iterate step ms)
  where
    -- compute minimum times on each axis independently
    go :: Int -> (Mono,Mono,Mono) -> (Int,Int,Int) -> [Moons] -> (Int,Int,Int)
    go i (seenx,seeny,seenz) (tx,ty,tz) (ms:mss)
      | tx < 0 || ty < 0 || tz < 0 = go (succ i) (seenx',seeny',seenz') (tx',ty',tz') mss
      | otherwise                  = (tx,ty,tz)
      where
        x = let [m0,m1,m2,m3] = map (fst3 *** fst3) (toList ms) in (m0,m1,m2,m3)
        seenx' | tx < 0    = S.insert x seenx
               | otherwise = seenx
        tx' = if tx < 0 && x `S.member` seenx then i else tx

        y = let [m0,m1,m2,m3] = map (snd3 *** snd3) (toList ms) in (m0,m1,m2,m3)
        seeny' | ty < 0    = S.insert y seeny
               | otherwise = seeny
        ty' = if ty < 0 && y `S.member` seeny then i else ty

        z = let [m0,m1,m2,m3] = map (trd3 *** trd3) (toList ms) in (m0,m1,m2,m3)
        seenz' | tz < 0    = S.insert z seenz
               | otherwise = seenz
        tz' = if tz < 0 && z `S.member` seenz then i else tz

