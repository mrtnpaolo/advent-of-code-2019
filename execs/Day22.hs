{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Advent
import qualified Data.List as L

import Text.Printf

main :: IO ()
main =
  do techniques <- parse <$> getRawInput 22
     print (L.findIndex (2019 ==) (shuffle techniques))

data T = New | Cut Int | Inc Int deriving (Show)

n :: Int
n = 10007 -- 10

new :: Int -> Int
new i = (n-1) - i

cut :: Int -> Int -> Int
cut k i = (i - k) `mod` n

inc :: Int -> Int -> Int
inc k i = k*i `mod` n

shuffle :: [T] -> [Int]
shuffle (combine -> f) = reorder (map f [0..n-1])
  where
    reorder = map fst . L.sortOn snd . zip [0..]

combine :: [T] -> Int -> Int
combine = compose . map eval

eval :: T -> (Int -> Int)
eval (New  ) = new
eval (Cut k) = cut k
eval (Inc k) = inc k

compose :: [(a -> a)] -> (a-> a)
compose = foldl (flip (.)) id

parse :: String -> [T]
parse = map (f . words) . lines
  where
    f xs | ["deal","into","new","stack"] <- xs = New
         | ["cut",read -> n] <- xs = Cut n
         | ["deal","with","increment",read -> n] <- xs = Inc n
    f _ = error "parse error"

t :: [(String,[Int])]
t =
  [(unlines ["deal with increment 7",
             "deal into new stack",
             "deal into new stack"]
   ,[0,3,6,9,2,5,8,1,4,7])
  ,(unlines ["cut 6",
             "deal with increment 7",
             "deal into new stack"]
   ,[3,0,7,4,1,8,5,2,9,6])
  ,(unlines ["deal with increment 7",
             "deal with increment 9",
             "cut -2"]
   ,[6,3,0,7,4,1,8,5,2,9])
  ,(unlines ["deal into new stack",
             "cut -2",
             "deal with increment 7",
             "cut 8",
             "cut -4",
             "deal with increment 7",
             "cut 3",
             "deal with increment 9",
             "deal with increment 3",
             "cut -1"]
   ,[9,2,5,8,1,4,7,0,3,6])
  ]

test :: IO ()
test = (flip mapM_) t $ \(raw,expected) ->
         do let realized = (shuffle . parse) raw
            printf "%s\texpected: %s\trealized: %s\n" (show $ expected == realized) (show expected) (show realized)