{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Data.Bifunctor

main :: IO ()
main =
  do print (energy (after 1000 moons))
     print t
  where
    after n = head . drop n . iterate step

    (tx,ty,tz) = part2 moons
    t = lcm (lcm tx ty) tz

type P = (Int,Int,Int) -- position
type V = (Int,Int,Int) -- velocity
type Moon = (P,V)
type Moons = (Moon,Moon,Moon,Moon)

moons :: Moons
moons =
  ( (( 4,  1, 1),(0,0,0))
  , ((11,-18,-1),(0,0,0))
  , ((-2,-10,-4),(0,0,0))
  , ((-7, -2,14),(0,0,0)) )

-- moons :: Moons
-- moons =
--   ( ((-1, 0,  2),(0,0,0))
--   , (( 2,-10,-7),(0,0,0))
--   , (( 4, -8, 8),(0,0,0))
--   , (( 3,  5,-1),(0,0,0)) )

opp :: (Int,Int,Int) -> (Int,Int,Int)
opp (!a,!b,!c) = (-a,-b,-c)

(<+>) :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
(!a,!b,!c) <+> (d,!e,!f) = (a+d,b+e,c+f)
{-# INLINE (<+>) #-}

gravity1 :: Int -> Int -> Int
gravity1 !a !b = case compare a b of LT -> -1; EQ -> 0; GT -> 1

gravity :: Moon -> Moon -> V
gravity ((!x0,!y0,!z0),_) ((!x1,!y1,!z1),_) =
  ( gravity1 x0 x1
  , gravity1 y0 y1
  , gravity1 z0 z1 )

effects :: Moons -> (V,V,V,V)
effects (!m0,!m1,!m2,!m3) =
  (     g01 <+>     g02 <+>     g03
  , opp g01 <+>     g12 <+>     g13
  , opp g02 <+> opp g12 <+>     g23
  , opp g03 <+> opp g13 <+> opp g23
  )
  where
    g01 = gravity m1 m0
    g02 = gravity m2 m0
    g03 = gravity m3 m0
    g12 = gravity m2 m1
    g13 = gravity m3 m1
    g23 = gravity m3 m2
{-# INLINE effects #-}

applyGravity :: Moons -> Moons
applyGravity ms@(!m0,!m1,!m2,!m3) = (second (v0 <+>) m0,second (v1 <+>) m1,second (v2 <+>) m2,second (v3 <+>) m3)
  where
    (!v0,!v1,!v2,!v3) = effects ms
{-# INLINE applyGravity #-}

applyVelocity :: Moons -> Moons
applyVelocity (!m0,!m1,!m2,!m3) = (f m0,f m1,f m2,f m3)
  where
    f ((!x,!y,!z),v@(!vx,!vy,!vz)) = ((x+vx,y+vy,z+vz),v)
{-# INLINE applyVelocity #-}

step :: Moons -> Moons
step = applyVelocity . applyGravity
{-# INLINE step #-}

energy :: Moons -> Int
energy (!m0,!m1,!m2,!m3) = e m0 + e m1 + e m2 + e m3
  where
    e m = p m * k m

    p ((!x,!y,!z),_) = abs x + abs y + abs z
    k (_,(!vx,!vy,!vz)) = abs vx + abs vy + abs vz

type Proj = (Int,Int,Int,Int,Int,Int,Int,Int)

project :: Moons -> (Proj,Proj,Proj)
project ms = (x,y,z)
  where
    (  ((!x0,!y0,!z0),(!vx0,!vy0,!vz0))
     , ((!x1,!y1,!z1),(!vx1,!vy1,!vz1))
     , ((!x2,!y2,!z2),(!vx2,!vy2,!vz2))
     , ((!x3,!y3,!z3),(!vx3,!vy3,!vz3)) ) = ms

    x = (x0,vx0,x1,vx1,x2,vx2,x3,vx3)
    y = (y0,vy0,y1,vy1,y2,vy2,y3,vy3)
    z = (z0,vz0,z1,vz1,z2,vz2,z3,vz3)
{-# INLINE project #-}

part2 :: Moons -> (Int,Int,Int) {- ^ Periods of repetition for each axis -}
part2 ms = go 1 0 0 0 (tail $ iterate step ms)
  where
    (startx,starty,startz) = project ms

    -- compute minimum times on each axis independently
    go :: Int -> Int -> Int -> Int -> [Moons] -> (Int,Int,Int)
    go !i !tx !ty !tz (!ms:mss)
      | tx < 1 || ty < 1 || tz < 1 = go (succ i) tx' ty' tz' mss
      | otherwise                  = (tx,ty,tz)
      where
        (x,y,z) = project ms
        tx' = if tx < 1 && startx == x then i else tx
        ty' = if ty < 1 && starty == y then i else ty
        tz' = if tz < 1 && startz == z then i else tz
