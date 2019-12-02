module Main where

import Advent

import Debug.Trace

import Data.List.Split

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

main =
  do raw <- map (read :: String -> Int) . lines . map c2n . init <$> getRawInput 2
     let ops = [ eval xs | xs <- chunksOf 4 raw ]
     -- mapM_ print ops
     let mem = alarm $ M.fromAscList $ zip [0..] raw
     -- pure $! traceShowId $! mem
     -- print $ run mem 0 M.! 0
     let space = [ ((noun, verb), run (setNoun noun . setVerb verb $ mem) 0)
                 | noun <- [0..99], verb <- [0..99] ]
     let solns = filter ((19690720 ==) . snd) space
     mapM_ print solns
  where
    c2n ',' = '\n'
    c2n  x  =  x
    alarm = setVerb 2 . setNoun 12
    setNoun = M.insert 1
    setVerb = M.insert 2

data IntCode = Halt | Sum Int Int Int | Mul Int Int Int | Noop
  deriving (Show)

eval (99:_) = Halt
eval [ 1,a,b,c] = Sum a b c
eval [ 2,a,b,c] = Mul a b c
eval _          = Noop

type Mem = M.Map Int Int

step :: Mem
     -> Int {- ^ instruction pointer -}
     -> (Mem,Bool) {- ^ (new memory, done) -}
step m i =
  case eval (take 4 . drop i . M.elems $ m) of
    Halt        -> {- traceShowId $! -} (m,True)
    Sum a' b' c -> {- traceShowId $! -} (M.insert c (a+b) m,False)
                     where a = m M.! a'
                           b = m M.! b'
    Mul a' b' c -> {- traceShowId $! -} (M.insert c (a*b) m,False)
                     where a = m M.! a'
                           b = m M.! b'
    Noop        -> {- traceShowId $! -} (m,True)

run :: Mem -> Int {- ^ instruction pointer -} -> Int
run m i = go m i False M.! 0
  where
    go m _ True = m
    go m i False = let (m',done) = step m i in go m' (i+4) done
