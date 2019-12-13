{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Advent
import Advent.IntCode
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import Control.Arrow
import Text.Printf
import System.IO
main :: IO ()
main =
  do mem <- map (read::String->Int) . words . map sep <$> getRawInput 13
     print (part1 mem)
     part2 (2 : tail mem) >>= print
  where sep ',' = ' '; sep x = x

part1 mem = M.size $ go M.empty (run' mem)
  where
    go m (Output x (Output y (Output tile eff))) =
      case tile of
        2 -> go (M.insertWith (+) (x,y) 1 m) eff
        _ -> go m eff
    go m (Input f) = error "dunno"
    go m Stop = m

part2 mem = arcade
  where
    arcade =
      do raw <- readFile "inputs/input13-human.txt"
         let ins = map (read::String->Int) (lines raw)
         printf "%s\n" (show ins)
         let save = appendFile "inputs/input13-human.txt" . printf "%d\n"
         go save (-1) M.empty ins (run' mem)
      where
        readInputs :: IO [Int]
        readInputs =
          withFile "inputs.txt" ReadMode $ \file ->
            map (read::String->Int) . lines <$> hGetContents file
    go save _     m ins    (Output (-1) (Output 0 (Output score eff))) = go save score m ins eff
    go save score m ins    (Output x    (Output y (Output tile  eff))) = go save score (M.insert (x,y) tile m) ins eff
    go save score m (x:xs) (Input f) = do -- putStr (showScreen score m)
                                          go save score m xs (f x)
    go save score m []     (Input f) = do putStr (showScreen score m)
                                          joystick <- input
                                          save joystick
                                          go save score m [] (f joystick)
    go save score m _      Stop = pure score

    input :: IO Int
    input =
      do c<-getChar
         case fromEnum c of
           10  -> pure 0  -- enter
           -- 117 -> pure 2 -- u
           27  -> do _ <- getChar
                     n <- getChar
                     case fromEnum n of
                       68 -> pure (-1) -- left
                       67 -> pure 1    -- right


showScreen :: Int -> Map (Int,Int) Int -> String
showScreen score m = (printf "Score: %d\n" score ++) . unlines $
  (flip map) [ym..yM] $ \y ->
    (flip map) [xm..xM] $ \x ->
      case m !? (x,y) of
        Nothing -> ' '
        Just 0 -> ' '
        Just 1 -> '#'
        Just 2 -> 'B'
        Just 3 -> '-'
        Just 4 -> 'o'
  where
    ((xm,xM),(ym,yM)) = (minimum &&& maximum) *** (minimum &&& maximum) $ unzip (M.keys m)
