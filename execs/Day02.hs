module Main (main) where

import Advent

import Data.List.Split (chunksOf)

import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as M

main :: IO ()
main =
  do raw <- map (read :: String -> Int) . words . map sep <$> getRawInput 2
     -- print $ (length raw, length raw `divMod` 4)
     -- print raw
     -- dumpProgram raw
     let mem = fromInts raw
     print $ part1 mem
     print `mapM_` part2 mem
  where
    sep ',' = ' '
    sep  x  =  x

part1 :: Mem -> Int
part1 = run . setup 12 2

part2 :: Mem -> [Int]
part2 mem
  = [ 100 * noun + verb | noun <- [0..99]
                        , verb <- [0..99]
                        , run (setup noun verb mem) == 19690720 ]

dumpProgram :: [Int] -> IO ()
dumpProgram raw = mapM_ print ops
  where
    ops = [ fromOpcode xs | xs <- chunksOf 4 raw ]

data IntCode
  = Halt
  | Sum Int Int Int
  | Mul Int Int Int
  | Unk
  deriving (Show)

fromOpcode :: [Int] -> IntCode
fromOpcode (99:_    ) = Halt
fromOpcode [ 1,a,b,c] = Sum a b c
fromOpcode [ 2,a,b,c] = Mul a b c
fromOpcode _          = Unk

type Mem = IntMap Int

fromInts :: [Int] -> Mem
fromInts = M.fromAscList . zip [0..]

step :: Mem -> IntCode -> Mem
step m (Sum a b c) = M.insert c (m!a + m!b) m
step m (Mul a b c) = M.insert c (m!a * m!b) m
step m Unk         = m                        -- unknown, unclear
step _ Halt        = error "executing Halt"

run :: Mem -> Int {- ^ value of memory location 0 at the end of the program -}
run m
  = go m 0 (fetch 0 m)
  where
    go m _ Halt = m ! 0
    go m i op   = let m' = step m op
                      i' = i+4
                  in go m' i' (fetch i' m)

    fetch :: Int -> Mem -> IntCode
    fetch i = fromOpcode . take 4 . drop i . M.elems

setup :: Int {- ^ noun -} -> Int {- ^ verb -} -> Mem -> Mem
setup noun verb = setNoun noun . setVerb verb
  where
    setNoun = M.insert 1
    setVerb = M.insert 2
