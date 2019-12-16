{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

import Advent

import Data.Char

main :: IO ()
main =
  do xs <- map digitToInt . init <$> getRawInput 16
     let ys = take 8 $ head $ drop 100 (iterate fft xs)
     print ys

base :: [Int]
base = [0,1,0,-1]

pattern :: Int -> [Int]
pattern n = cycle (base >>= replicate n)

fft :: [Int] -> [Int]
fft xs = zipWith dot (repeat xs) $ take (length xs) (map (take (length xs) . tail . pattern) [1..])

dot :: [Int] -> [Int] -> Int
dot xs ys = (\n -> abs n `mod` 10) (sum (zipWith (*) xs ys))
