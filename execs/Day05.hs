module Main (main) where

import Advent

import Data.List (unfoldr)
import Data.Maybe (fromMaybe)

import Data.IntMap.Strict (IntMap, (!?))
import qualified Data.IntMap.Strict as M

import Control.Monad.Trans.RWS.CPS (RWS)
import qualified Control.Monad.Trans.RWS.CPS as RWS

-- import Debug.Trace

main :: IO ()
main =
  do mem <- fromInts . map read . words . map sep <$> getRawInput 5
     print $ last (run mem [1])
     print $ last (run mem [5])
  where
    sep ',' = ' '
    sep x = x

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

-- | Machine's memory
type Mem = IntMap Int

fromInts :: [Int] -> Mem
fromInts = M.fromAscList . zip [0..]

-- | Running a program
run :: Mem -> [Int] {- ^ Inputs -} -> [Int] {- ^ Outputs -}
run mem ins = outs
  where
    (_,outs) = RWS.execRWS go undefined (Machine { _mem = mem, _ip = 0, _ins = ins })

    go = do ~(ip,(opcode,modes),params) <- fetch
            case opcode of
              Unk  -> error $ "unknown instruction: opcode=" ++ show opcode ++ " (ip=" ++ show ip ++ ")"
              Halt -> pure ()
              _    -> exec opcode (params @@ modes) >> go

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
exec instr@Inp   (a:_)     = ask >>= saveAt a >> next instr
exec instr@Out   (a:_)     = at a >>= RWS.tell . pure >> next instr
exec instr@JT    (a:b:_)   = at a >>= \n -> if n /= 0 then at b >>= \m -> setIp m else next instr
exec instr@JF    (a:b:_)   = at a >>= \n -> if n == 0 then at b >>= \m -> setIp m else next instr
exec instr@Sum   (a:b:c:_) = (+) <$> at a <*> at b >>= saveAt c >> next instr
exec instr@Mul   (a:b:c:_) = (*) <$> at a <*> at b >>= saveAt c >> next instr
exec instr@Less  (a:b:c:_) = (<)  <$> at a <*> at b >>= \cond -> saveAt c (if cond then 1 else 0) >> next instr
exec instr@Equal (a:b:c:_) = (==) <$> at a <*> at b >>= \cond -> saveAt c (if cond then 1 else 0) >> next instr
exec       Halt  _         = error "exec: Halt"
exec       Unk   _         = error "exec: Unk"

-- | IntCode machine state
data Machine = Machine { _mem :: Mem, _ip :: Int, _ins :: [Int] }

-- | IntCode machine operation
type OpEnv = ()
type OpLog = [Int]
type OpState = Machine
type Op = RWS OpEnv OpLog OpState

-- | Read a modal parameter
at :: Mode Int -> Op Int
at (Imm x)  = pure x
at (Pos px) = RWS.gets (fromMaybe (error "memory access out of bounds") . (!? px) . _mem)

-- | Write to a modal parameter
saveAt :: Mode Int -> Int -> Op ()
saveAt mx n
  = do let x = naked mx
       -- traceM ("saving " ++ show n ++ " at memory location " ++ show x)
       RWS.modify (\m -> m { _mem = M.insert x n (_mem m) })

-- | Consume one input number
ask :: Op Int
ask = RWS.state (\m -> let (x:xs) = _ins m in (x, m { _ins = xs }))

-- | Increment the instruction pointer
incrIp :: Int -> Op ()
incrIp n = RWS.gets _ip >>= setIp . (n +)

-- | Set the instruction pointer
setIp :: Int -> Op ()
setIp n = RWS.modify (\m -> m { _ip = n })

-- | Advance the instruction pointer to the next instruction
next :: IntCode -> Op ()
next = incrIp . len
