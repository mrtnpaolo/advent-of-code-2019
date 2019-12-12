{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Data.List

main :: IO ()
main =
  do print (energy (after 10 moons))
     print (part2 moons)
  where
    after n = head . drop n . iterate step

type P = [Int] -- position
type V = [Int] -- velocity
type Moon = (P,V)
type Moons = [Moon]

moons :: Moons
moons =
  [ ( [ 4,  1, 1] , [0,0,0] )
  , ( [11,-18,-1] , [0,0,0] )
  , ( [-2,-10,-4] , [0,0,0] )
  , ( [-7, -2,14] , [0,0,0] ) ]

-- moons :: Moons
-- moons =
--   [ ( [-1, 0,  2] , [0,0,0] )
--   , ( [ 2,-10,-7] , [0,0,0] )
--   , ( [ 4, -8, 8] , [0,0,0] )
--   , ( [ 3,  5,-1] , [0,0,0] ) ]

(<+>) :: [Int] -> [Int] -> [Int]
u <+> v = zipWith (+) u v
{-# INLINE (<+>) #-}

gravity :: P -> P -> V
gravity !p !q = zipWith comp p q
  where
    comp !a !b = case compare a b of LT -> -1; EQ -> 0; GT -> 1
{-# INLINE gravity #-}

applyGravity :: Moons -> Moons
applyGravity ms = [ (x,foldl' (<+>) v [ gravity y x | (y,_) <- ms ]) | (x,v) <- ms ]

applyVelocity :: Moons -> Moons
applyVelocity ms = [ (x <+> v,v) | (x,v) <- ms ]

step :: Moons -> Moons
step = applyVelocity . applyGravity

energy :: Moons -> Int
energy = sum . map e
  where
    e m = p m * k m

    p (x,_) = sum (map abs x)
    k (_,v) = sum (map abs v)

part2 :: Moons -> Int
part2 ms = (2*) . lcm' . map (succ . findRepeat) . byDimension . velocities . iterate step $ ms
  where
    velocities = map (map snd)
    byDimension = take dims . transpose . map transpose
    dims = length.fst.head $ ms
    lcm' = foldl' lcm 1

findRepeat :: Eq a => [a] -> Int
findRepeat [] = error "empty list"
findRepeat (x:xs) = let Just i = elemIndex x xs in i
