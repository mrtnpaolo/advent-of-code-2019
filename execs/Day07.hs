{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main (main) where

import Advent

import Data.List (unfoldr, permutations)
import Data.Maybe (fromMaybe)

import Data.IntMap.Strict (IntMap, (!?))
import qualified Data.IntMap.Strict as M

import Control.Monad.Trans.RWS.CPS (RWS)
import qualified Control.Monad.Trans.RWS.CPS as RWS

main :: IO ()
main =
  do mem <- fromInts . map read . words . map sep <$> getRawInput 7
     print $ part1 mem
     print $ part2 mem
  where
    sep ',' = ' '
    sep x = x

part1 :: Mem -> Int
part1 mem = maximum [ thrusters mem phases | phases <- permutations [0..4] ]

thrusters :: Mem -> [Int] -> Int
thrusters mem [a,b,c,d,e] = e'
  where [a'] = run mem [a,0 ]
        [b'] = run mem [b,a']
        [c'] = run mem [c,b']
        [d'] = run mem [d,c']
        [e'] = run mem [e,d']

part2 :: Mem -> Int
part2 mem = maximum [ last $ last $ feedback mem phases | phases <- permutations [5..9] ]

feedback :: Mem -> [Int] {- ^ Phases -} -> [[Int]]
feedback mem [pa,pb,pc,pd,pe] = outs
  where
    [a,b,c,d,e] = effectuate . eval . load <$> replicate 5 mem

    outs = let a' = a (pa:0:e')
               b' = b (pb:a')
               c' = c (pc:b')
               d' = d (pd:c')
               e' = e (pe:d')
           in [a',b',c',d',e']

-- | Running a program
run :: Mem -> [Int] {- ^ Inputs -} -> [Int] {- ^ Outputs -}
run = effectuate . eval . load

-- $ Effects

data Effect
  = Output Int Effect
  | Input (Int -> Effect)
  | Stop

-- | Compute the outputs of an effect with the given list of inputs
effectuate :: Effect -> [Int] {- ^ Inputs -} -> [Int] {- ^ Outputs -}
effectuate Stop         _      = []
effectuate (Output n e) ins    = n : effectuate e ins
effectuate (Input _)    []     = error "starved"
effectuate (Input f)    (x:xs) = effectuate (f x) xs

-- | Turn a machine into its effect
eval :: Machine -> Effect
eval m =
  case op of

    Halt -> Stop

    Out  -> let ( m', [out] ) = RWS.execRWS (exec op (ps @@ ms)) undefined m
            in Output out (eval m')

    Inp  -> Input $ \n ->
              let m' = m { _ins = _ins m ++ [n] }
                  ( m'', _ ) = RWS.execRWS (exec op (ps @@ ms)) undefined m'
              in eval m''

    _    -> let ( m' , _ ) = RWS.execRWS (exec op (ps @@ ms)) undefined m
            in eval m'

  where
    ( ( _,(op,ms),ps ) , _ , _ ) = RWS.runRWS fetch undefined m

-- $ Machine

-- | Machine's memory
type Mem = IntMap Int

fromInts :: [Int] -> Mem
fromInts = M.fromAscList . zip [0..]

-- | Machine state
data Machine = Machine { _mem :: Mem, _ip :: Int, _ins :: [Int] }

-- | Load memory into a machine
load :: Mem -> Machine
load mem = Machine { _mem = mem, _ip = 0, _ins = [] }

-- $ IntCode

-- | Parameter modes
data Mode a
  = Pos a -- position mode (pointer)
  | Imm a -- immediate mode (value)

naked :: Mode a -> a
naked (Pos a) = a
naked (Imm a) = a

instance Show a => Show (Mode a) where
  showsPrec _ (Pos x) = shows x
  showsPrec _ (Imm x) = showChar '*' . shows x

-- | IntCode
data IntCode
  = Unk | Sum | Mul | Inp | Out | JT | JF | Less | Equal | Halt
  deriving (Show)

-- | IntCode instruction from opcode
code :: Int -> IntCode
code  1 = Sum
code  2 = Mul
code  3 = Inp
code  4 = Out
code  5 = JT
code  6 = JF
code  7 = Less
code  8 = Equal
code 99 = Halt
code  _ = Unk

-- | IntCode instruction lengths
len :: IntCode -> Int
len Unk   = 1
len Sum   = 4
len Mul   = 4
len Inp   = 2
len Out   = 2
len JT    = 3
len JF    = 3
len Less  = 4
len Equal = 4
len Halt  = 1

-- | IntCode machine operation
type OpEnv   = ()
type OpLog   = [Int]
type OpState = Machine
type Op      = RWS OpEnv OpLog OpState

-- | Fetch the instruction pointer and the decoded instruction
fetch :: Op (Int,(IntCode,[Int]),[Int]) -- {- ^ (IP, decoded istruction, parameters) -}
fetch
  = do ip <- RWS.gets _ip
       ~(encoded:params) <- RWS.gets (drop ip . M.elems . _mem)
       pure (ip,decode encoded,params)

-- | Decode an instruction into its IntCode and parameter modes
decode :: Int -> (IntCode,[Int])
decode n = (opcode,modes)
  where
    (m,r) = n `divMod` 100
    opcode = code r
    modes  = unfoldr (Just . swap . (`divMod` 10)) m
      where
        swap (x,y) = (y,x)

-- | Decorate parameters with their respective modes
(@@) :: [Int] {- ^ parameters -} -> [Int] {- ^ modes -} -> [Mode Int]
ps @@ ms = zipWith (\case 0 -> Pos; 1 -> Imm; _ -> error "unknown mode") ms ps

-- | Execute one Intcode operation
exec :: IntCode -> [Mode Int] {- ^ modal parameters -} -> Op ()

exec instr@Inp   (a:_)     = do set a =<< ask
                                next instr

exec instr@Out   (a:_)     = do RWS.tell . pure =<< get a
                                next instr

exec instr@JT    (a:b:_)   = do n <- get a
                                if n /= 0
                                  then setIp =<< get b
                                  else next instr

exec instr@JF    (a:b:_)   = do n <- get a
                                if n == 0
                                  then setIp =<< get b
                                  else next instr

exec instr@Sum   (a:b:c:_) = do set c =<< (+) <$> get a <*> get b
                                next instr

exec instr@Mul   (a:b:c:_) = do set c =<< (*) <$> get a <*> get b
                                next instr

exec instr@Less  (a:b:c:_) = do set c =<< fmap fromEnum ((<) <$> get a <*> get b)
                                next instr

exec instr@Equal (a:b:c:_) = do set c =<< fmap fromEnum ((==) <$> get a <*> get b)
                                next instr

exec       Halt  _         = error "exec: Halt"

exec       Unk   _         = error "exec: Unk"

-- | Read a modal parameter
get :: Mode Int -> Op Int
get (Imm x)  = pure x
get (Pos px) = RWS.gets (fromMaybe (error "memory access out of bounds") . (!? px) . _mem)

-- | Write to a modal parameter
set :: Mode Int -> Int -> Op ()
set mx n = RWS.modify (\m -> m { _mem = M.insert (naked mx) n (_mem m) })

-- | Consume one input number
ask :: Op Int
ask = RWS.state $ \m ->
  case (_ins m) of
    []     -> error "starving for input"
    (x:xs) -> (x, m { _ins = xs })

-- | Increment the instruction pointer
incrIp :: Int -> Op ()
incrIp n = RWS.gets _ip >>= setIp . (n +)

-- | Set the instruction pointer
setIp :: Int -> Op ()
setIp n = RWS.modify (\m -> m { _ip = n })

-- | Advance the instruction pointer to the next instruction
next :: IntCode -> Op ()
next = incrIp . len
