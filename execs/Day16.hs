{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

import Advent

import Data.Char

import Data.Vector (Vector)
import qualified Data.Vector as V

import Debug.Trace

main :: IO ()
main =
  do -- xs <- map digitToInt . init <$> getRawInput 16
     -- print (part1 xs)
     -- let t1 = "03036732577212944063491565474664"
     -- print (part2 t1)
     -- xs <- init <$> getRawInput 16
     let xs = "03036732577212944063491565474664"
     print (part2 xs)

-- part1 :: [Int] -> Vector Int
-- part1 = V.take 8 . after 100 fft' . V.fromList

-- part2 raw = V.slice (offset+1) 8 (let ys = fft' xs in ys `seq` fft' ys)
part2 raw = V.slice (offset+1) 8 (after 100 fft'' xs)
  where
    offset = read (take 7 raw) :: Int

    digits = map digitToInt raw

    stem = V.generate (length digits) (digits !!)

    xs = V.concat (replicate 10000 stem)

pattern' :: Int -> Int -> Int
pattern' i j =
  case (j `mod` (i*4)) `div` i of
    0 -> 0
    1 -> 1
    2 -> 0
    3 -> -1

indices len i = let them = takeWhile (< len) [ k*i + n`mod`i | n <- [0..], let j = (n `div` i), let k = (j+1)*2 - 1 ]
  in them `seq` traceShow (length them) them

-- this is faster because it doesn't fold over the whole thing every iteration
-- but it only accesses a decreasing number of indices as it churns along
fft'' :: Vector Int -> Vector Int
fft'' xs = V.generate len f
  where
    len = V.length xs
    f i = let y = (\n -> abs n `mod` 10) $ sum [ xs V.! i' * pattern' (i+1) i' | i' <- indices len (i+1) ]
          in y `seq` y

-- the fold might be making this too slow, trying to generate just the non-zero indices
-- pattern' :: Int -> Int -> Int
-- pattern' i j =
--   case (j `mod` (i*4)) `div` i of
--     0 -> 0
--     1 -> 1
--     2 -> 0
--     3 -> -1
-- fft' :: Vector Int -> Vector Int
-- fft' xs = V.generate (V.length xs) f
--   where
--     f i = (\n -> abs n `mod` 10) (V.ifoldl' g 0 xs)
--       where
--         g acc j x = acc + (pattern' (i+1) (j+1)) * x

-- list approach is a no-no
-- part2 raw = take 8 . drop offset . after 100 fft $ take (10000 * length xs) (cycle xs)
--   where
--     xs = map digitToInt raw
--     offset = read (take 7 raw) :: Int

after n f = head . drop n . iterate f

base :: [Int]
base = [0,1,0,-1]

pattern :: Int -> [Int]
pattern n = cycle (base >>= replicate n)

fft :: [Int] -> [Int]
fft xs = zipWith dot (repeat xs) $ take (length xs) (map (take (length xs) . tail . pattern) [1..])

dot :: [Int] -> [Int] -> Int
dot xs ys = (\n -> abs n `mod` 10) (sum (zipWith (*) xs ys))
