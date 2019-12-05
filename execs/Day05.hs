module Main (main) where

import Debug.Trace

import Advent

import Data.IntMap.Strict (IntMap, (!), (!?))
import qualified Data.IntMap.Strict as M

main :: IO ()
main =
  do mem <- fromInts . map read . words . map sep <$> getRawInput 5
     -- (print :: Int -> IO ()) `mapM_` raw
     print $ run mem
  where
    sep ',' = ' '
    sep x = x

data IntCode
  = Halt
  | Sum (Mode Int) (Mode Int) (Mode Int)
  | Mul (Mode Int) (Mode Int) (Mode Int)
  | Inp (Mode Int)
  | Out (Mode Int)
  deriving (Show)

data Mode a = Pos a | Imm a
  deriving (Show)


readInstruction :: [Int]
                -> (IntCode,Int) {- ^ (instruction, instruction length) -}
readInstruction (99:_) = ( Halt, 1 )
readInstruction (n:ns)
  | [1,ma,mb,mc] <- decode n = let [a,b,c] = take 3 ns in ( Sum (mode ma a) (mode mb b) (mode mc c), 4 )
  | [2,ma,mb,mc] <- decode n = let [a,b,c] = take 3 ns in ( Mul (mode ma a) (mode mb b) (mode mc c), 4 )
  | [3,ma,_,_]   <- decode n = let [a]     = take 1 ns in ( Inp (mode ma a), 2 )
  | [4,ma,_,_]   <- decode n = let [a]     = take 1 ns in ( Out (mode ma a), 2 )
  | otherwise = error $ "cannot decode instruction: " ++ show n
  where
    mode :: Int -> Int -> Mode Int
    mode 0 = Pos
    mode 1 = Imm
    mode _ = error "unsupported memory mode"



type Mem = IntMap Int

fromInts :: [Int] -> Mem
fromInts = M.fromAscList . zip [0..]

run :: Mem -> Int {- ^ value of memory location 0 at the end of the program -}
run m
  = go m 0 (fetch 0 m)
  where
    go m _ (Halt,_) = m ! 0
    go m i (op,len) = let m' = step m (traceShowId op)
                          i' = traceShowId $ i+len
                      in go m' i' (fetch i' m')

    fetch :: Int -> Mem -> (IntCode,Int)
    fetch i = readInstruction . drop i . M.elems



-- | Parameter lookup via the correct mode
-- Memory access can create new elements in memory!
(!~) :: Mem -> Mode Int -> (Mem,Int)
m !~ (Pos n) | Just x <- m !? n = (m,x)
             | otherwise        = (M.insert n 0 m,0)
m !~ (Imm n) = (m,n)


step :: Mem -> IntCode -> Mem
step _ (Sum _ _ (Imm _)) = error "cannot write Sum to immediate parameter"
step _ (Mul _ _ (Imm _)) = error "cannot write Mul to immediate parameter"
step m (Sum a b (Pos c)) = M.insert c (a' + b') m''
  where
    (m' ,a') = m  !~ a
    (m'',b') = m' !~ b
step m (Mul a b (Pos c)) = M.insert c (a' * b') m''
  where
    (m' ,a') = m  !~ a
    (m'',b') = m' !~ b
step _ (Inp (Imm _)) = error "cannot write Inp to immediate parameter"
step m (Inp (Pos a)) = M.insert a 1 m
step m (Out ma) = trace ("Out: " ++ (show $ snd $ m !~ ma)) m

step _ Halt        = error "executing Halt"







-- what the hell are the instructions @_@

-- | 0 = Position Mode = pointer
--   1 = Immediate Mode = value
decode :: Int -> [Int]
decode = map snd . take 4 . iterate ((`divMod` 10) . fst) . (`divMod` 100)

test_decode :: Bool
test_decode = and $ zipWith (==)
  [ decode 1002, decode 1  ]
  [ [2,0,1,0]  , [1,0,0,0] ]


-- writes are never to an immediate mode parameter

-- ip += instruction length (can be diff than 4)
-- support negative integers

-- one input (1 = air conditioner)
-- many outputs (delta from correct, 0 is correct)
-- diagnostic code (whatever)