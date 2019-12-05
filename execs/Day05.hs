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

-- | Non-Mode params are always positional (because they are write locations)
data IntCode
  = Halt
  | Sum (Mode Int) (Mode Int) Int
  | Mul (Mode Int) (Mode Int) Int
  | Inp Int
  | Out (Mode Int)
  | JT (Mode Int) (Mode Int)     -- jump-if-true
  | JF (Mode Int) (Mode Int)     -- jump-if-false
  | Less (Mode Int) (Mode Int) Int -- less than
  | Equal (Mode Int) (Mode Int) Int -- equal
  deriving (Show)

data Mode a = Pos a | Imm a
  deriving (Show)


readInstruction :: [Int]
                -> (IntCode,Int) {- ^ (instruction, instruction length) -}
readInstruction [] = error "cannot decode empty instruction"
readInstruction (99:_) = ( Halt, 1 )
readInstruction (n:ns)
  | [1,ma,mb,_] <- decode n = let [a,b,pc] = take 3 ns in ( Sum (mode ma a) (mode mb b) pc, 4 )
  | [2,ma,mb,_] <- decode n = let [a,b,pc] = take 3 ns in ( Mul (mode ma a) (mode mb b) pc, 4 )
  | [3,_,_,_]   <- decode n = let [pa]     = take 1 ns in ( Inp pa, 2 )
  | [4,ma,_,_]  <- decode n = let [a]      = take 1 ns in ( Out (mode ma a), 2 )
  | [5,ma,mb,_] <- decode n = let [a,b]    = take 2 ns in ( JT (mode ma a) (mode mb b), 3 )
  | [6,ma,mb,_] <- decode n = let [a,b]    = take 2 ns in ( JF (mode ma a) (mode mb b), 3 )
  | [7,ma,mb,_] <- decode n = let [a,b,pc] = take 3 ns in ( Less (mode ma a) (mode mb b) pc, 4 )
  | [8,ma,mb,_] <- decode n = let [a,b,pc] = take 3 ns in ( Equal (mode ma a) (mode mb b) pc, 4 )
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
    go m i (op,len) = let (m',jump) = step m (traceShowId op)
                          i' | jump /= 0 = jump
                             | otherwise = i+len
                      in go m' i' (fetch i' m')

    fetch :: Int -> Mem -> (IntCode,Int)
    fetch i = readInstruction . drop i . M.elems



-- | Parameter lookup via the correct mode
-- Memory access can create new elements in memory!
(!~) :: Mem -> Mode Int -> (Mem,Int)
m !~ (Pos n) | Just x <- m !? n = (m,x)
             | otherwise        = (M.insert n 0 m,0)
m !~ (Imm n) = (m,n)


step :: Mem -> IntCode -> (Mem,Int) {- ^ resulting memory, optional nonzero jump instruction pointer -}
step m (Sum a b pc) = ( M.insert pc (a' + b') m'', 0 )
  where
    (m' ,a') = m  !~ a
    (m'',b') = m' !~ b
step m (Mul a b pc) = ( M.insert pc (a' * b') m'', 0 )
  where
    (m' ,a') = m  !~ a
    (m'',b') = m' !~ b
step m (Inp pa) = ( M.insert pa 5 {- XXX: thermal radiator controller -} m, 0 )
step m (Out ma) = ( trace ("Out: " ++ (show $ snd $ m !~ ma)) m, 0 )
step m (JT a b) = ( m, if 0 /= snd (m !~ a) then snd (m !~ b) else 0 )
step m (JF a b) = ( m, if 0 == snd (m !~ a) then snd (m !~ b) else 0 )
step m (Less a b pc) = ( M.insert pc v m'', 0 )
  where
    (m' ,a') = m  !~ a
    (m'',b') = m' !~ b
    v | a' < b' = 1 | otherwise = 0
step m (Equal a b pc) = ( M.insert pc v m'', 0 )
  where
    (m' ,a') = m  !~ a
    (m'',b') = m' !~ b
    v | a' == b' = 1 | otherwise = 0
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