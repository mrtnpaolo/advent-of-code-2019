{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures -Wno-incomplete-patterns #-}
module Main (main) where

import Advent

import Data.Char

import Data.IntSet (IntSet)
import qualified Data.IntSet as S

import Data.Map (Map)
import qualified Data.Map as M

-- import Data.Vector.Unboxed (Vector)
-- import qualified Data.Vector.Unboxed as V
-- import qualified Data.Vector.Unboxed.Mutable as MV

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
     -- let xs = "12345678"
     -- print `mapM_` (take 5 $ part2 xs)

-- part1 :: [Int] -> Vector Int
-- part1 = V.take 8 . after 100 fft' . V.fromList

-- [ [ (j,k) | j <- [1..8], let k = coeff i j, k /= 0 ] | i <- [1..8] ]


part2 raw = M.elems (go 100 goal)
  where
    digits = map digitToInt raw
    len    = length digits

    offset = read (take 7 raw) :: Int

    count = 10000
    total = count * len

    goal = S.fromList [ offset + i | i <- [1..8] ]

    digit i = digits !! ((i-1) `mod` len)

    go :: Int -> IntSet -> Map Int Int
    go 0 wanted = M.fromList [ (i,digit i) | i <- S.toList wanted ]
    go n wanted = result
      where
        result = M.fromList [ (i,f upward i) | i <- S.toList wanted ]
        needed = S.unions [ S.fromList (ix total i) | i <- S.toList wanted ]
        upward = go (n-1) (traceShow (S.size needed) needed)
        f :: Map Int Int -> Int -> Int
        f m i = lastDigit (sum [ (m M.! j) * weight i j | j <- ix total i ])

-- | 1-based matrix of weights (i is the row, j is the column)
weight :: Int -> Int -> Int
weight i j =
  case (j `mod` (4*i)) `div` i of
    0 -> 0
    1 -> 1
    2 -> 0
    3 -> -1

-- | indices needed to compute i from a list of length `len`
-- i.e. the positions of the elements of non-null weight needed (1-based)
ix len i = takeWhile (<= len) [ k*i + n`mod`i | n <- [0..], let j = (n `div` i), let k = (j+1)*2 - 1 ]

lastDigit n = (abs n) `mod` 10



-- -- part2 raw = V.slice (offset+1) 8 (let ys = fft' xs in ys `seq` fft' ys)
-- part2 raw = V.slice 0 8 (fft ys) -- (after 100 fft ys)
--   where
--     count = 10000
--     offset = read (take 7 raw) :: Int

--     digits = map digitToInt raw
--     stem = V.generate (length digits) (digits !!)
--     xs = V.concat (replicate count stem)
--     ys = V.drop offset xs
--     len = V.length ys

--     fft = fft4 offset len

-- test raw count offset = fft4 offset len ys
--   where
--     digits = map digitToInt raw
--     stem = V.generate (length digits) (digits !!)
--     xs = V.concat (replicate count stem)
--     ys = V.drop offset xs
--     len = V.length ys


-- pattern' :: Int -> Int -> Int
-- pattern' i j =
--   case (j `mod` (i*4)) `div` i of
--     0 -> 0
--     1 -> 1
--     2 -> 0
--     3 -> -1

-- indices len i = let them = takeWhile (<= len) [ k*i + n`mod`i | n <- [0..], let j = (n `div` i), let k = (j+1)*2 - 1 ]
--   in them
--   -- in them `seq` traceShow (length them) them

-- fft4 :: Int -> Int -> Vector Int -> Vector Int
-- fft4 offset len v = go 0 v
--   where

--     go i xs

--       | i == len  = xs

--       | otherwise = go (i+1) ys
--         where
--           ys = V.modify (\v -> MV.write v i digit) xs
--           digit = abs n `mod` 10
--           n = sum [ (xs V.! (j - offset - 1)) * pattern' (offset + (i+1)) j | j <- indices (offset + len) (offset + (i+1)) ]









-- -- I can drop the beginning!!!!

-- -- -- part2 raw = V.slice (offset+1) 8 (let ys = fft' xs in ys `seq` fft' ys)
-- -- part2 raw = V.slice (offset+1) 8 (fft''' xs) -- (after 100 fft''' xs)
-- --   where
-- --     offset = read (take 7 raw) :: Int

-- --     digits = map digitToInt raw

-- --     stem = V.generate (length digits) (digits !!)

-- --     xs = V.concat (replicate 10000 stem)


-- -- -- part2 raw = iterate fft''' xs -- (after 100 fft''' xs)
-- -- --   where
-- -- --     digits = map digitToInt raw
-- -- --     xs = V.generate (length digits) (digits !!)

-- -- pattern' :: Int -> Int -> Int
-- -- pattern' i j =
-- --   case (j `mod` (i*4)) `div` i of
-- --     0 -> 0
-- --     1 -> 1
-- --     2 -> 0
-- --     3 -> -1

-- -- indices len i = let them = takeWhile (<= len) [ k*i + n`mod`i | n <- [0..], let j = (n `div` i), let k = (j+1)*2 - 1 ]
-- --   in them
-- --   -- in them `seq` traceShow (length them) them

-- -- fft''' :: Vector Int -> Vector Int
-- -- fft''' v = go 0 v
-- --   where
-- --     len = V.length v

-- --     go i xs

-- --       | i == len  = xs

-- --       | otherwise = go (i+1) ys
-- --         where
-- --           ys = V.modify (\v -> MV.write v i digit) xs
-- --           digit = abs n `mod` 10
-- --           n = sum [ xs V.! (j-1) * pattern' (i+1) j | j <- indices len (i+1) ]




-- -- this is faster because it doesn't fold over the whole thing every iteration
-- -- but it only accesses a decreasing number of indices as it churns along
-- -- fft'' :: Vector Int -> Vector Int
-- -- fft'' xs = V.generate len f
-- --   where
-- --     len = V.length xs
-- --     f i = let y = (\n -> abs n `mod` 10) $ sum [ xs V.! i' * pattern' (i+1) i' | i' <- indices len (i+1) ]
-- --           in y `seq` y

-- -- the fold might be making this too slow, trying to generate just the non-zero indices
-- -- pattern' :: Int -> Int -> Int
-- -- pattern' i j =
-- --   case (j `mod` (i*4)) `div` i of
-- --     0 -> 0
-- --     1 -> 1
-- --     2 -> 0
-- --     3 -> -1
-- -- fft' :: Vector Int -> Vector Int
-- -- fft' xs = V.generate (V.length xs) f
-- --   where
-- --     f i = (\n -> abs n `mod` 10) (V.ifoldl' g 0 xs)
-- --       where
-- --         g acc j x = acc + (pattern' (i+1) (j+1)) * x

-- -- list approach is a no-no
-- -- part2 raw = take 8 . drop offset . after 100 fft $ take (10000 * length xs) (cycle xs)
-- --   where
-- --     xs = map digitToInt raw
-- --     offset = read (take 7 raw) :: Int

-- after n f = head . drop n . iterate f

-- base :: [Int]
-- base = [0,1,0,-1]

-- pattern :: Int -> [Int]
-- pattern n = cycle (base >>= replicate n)

-- fft :: [Int] -> [Int]
-- fft xs = zipWith dot (repeat xs) $ take (length xs) (map (take (length xs) . tail . pattern) [1..])

-- dot :: [Int] -> [Int] -> Int
-- dot xs ys = (\n -> abs n `mod` 10) (sum (zipWith (*) xs ys))
