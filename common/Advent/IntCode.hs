{-# LANGUAGE LambdaCase #-}
module Advent.IntCode where

import Data.List (unfoldr)
import Data.Maybe (fromMaybe, listToMaybe)

import Data.IntMap.Strict (IntMap, (!), (!?))
import qualified Data.IntMap.Strict as M

import Control.Monad.Trans.RWS.CPS (RWS)
import qualified Control.Monad.Trans.RWS.CPS as RWS

-- | Running a program
run :: [Int] -> [Int] {- ^ Inputs -} -> [Int] {- ^ Outputs -}
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

-- | Turn a machine Into its effect
eval :: Machine -> Effect
eval m =
  case (peek m) of
    Halt -> Stop
    Out  -> let  ( m', Just out ) = exec1 m undefined  in  Output out (eval m')
    Inp  -> Input $ \x ->
            let  ( m', Nothing  ) = exec1 m x          in  eval m'
    _    -> let  ( m', Nothing  ) = exec1 m undefined  in  eval m'

-- $ Machine

-- | Machine's memory
type Mem = IntMap Int

-- | Machine state
data Machine = Machine { _mem :: Mem, _rb :: Int, _ip :: Int, _ins :: [Int] }

-- | Load memory Into a machine
load :: [Int] -> Machine
load mem = Machine { _mem = M.fromAscList (zip [0..] mem), _rb = 0, _ip = 0, _ins = [] }

-- | Peek at the current operation
peek :: Machine -> IntCode
peek m = fst . decode . head . drop ip . M.elems . _mem $ m
  where
    ip = _ip m

-- $ IntCode

-- | Parameter modes
data Mode a
  = Pos a -- position mode (absolute pointer)
  | Imm a -- immediate mode (value)
  | Rel a -- relative mode (relative pointer)

naked :: Mode a -> a
naked (Pos a) = a
naked (Imm a) = a
naked (Rel a) = a

instance Show a => Show (Mode a) where
  showsPrec _ (Pos x) = shows x
  showsPrec _ (Imm x) = showChar '*' . shows x

-- | IntCode
data IntCode
  = Unk | Sum | Mul | Inp | Out | JT | JF | Less | Equal | Arb | Halt
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
code  9 = Arb
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
len Arb   = 2
len Halt  = 1

-- | Decode an instruction Into its IntCode and parameter modes
decode :: Int -> (IntCode,[Int])
decode n = (opcode,modes)
  where
    (m,r) = n `divMod` 100
    opcode = code r
    modes  = unfoldr (Just . swap . (`divMod` 10)) m
      where
        swap (x,y) = (y,x)

-- $ IntCode machine

-- | IntCode machine operation
type OpEnv   = Int
type OpLog   = [Int]
type OpState = Machine
type Op      = RWS OpEnv OpLog OpState

-- | Execute one IntCode operation
exec1 :: Machine -> Int -> (Machine,Maybe Int {- ^ Output -})
exec1 m x = listToMaybe <$> RWS.execRWS (fetch >>= uncurry eval1) x m

-- | Fetch the current opcode
fetch :: Op (IntCode,[Mode Int]) -- {- ^ (decoded istruction, modal parameters) -}
fetch =
  do ip <- RWS.gets _ip
     ~(encoded:params) <- RWS.gets ((\mem -> (mem !) <$> [ip..]) . _mem)
     let ( opcode, modes ) = decode encoded
     pure ( opcode, params @@ modes )
  where
    -- | Decorate parameters with their respective modes
    (@@) :: [Int] {- ^ parameters -} -> [Int] {- ^ modes -} -> [Mode Int]
    ps @@ ms = zipWith (\case 0 -> Pos; 1 -> Imm; 2 -> Rel; _ -> error "unknown mode") ms ps

-- | Evaluate one IntCode operation
eval1 :: IntCode -> [Mode Int] {- ^ modal parameters -} -> Op ()

eval1 i@Inp   (a:_)     = do set a =<< ask
                             next i

eval1 i@Out   (a:_)     = do RWS.tell . pure =<< get a
                             next i

eval1 i@Arb   (a:_)     = do adjustRelativeBase =<< get a
                             next i

eval1 i@JT    (a:b:_)   = do n <- get a
                             if n /= 0
                               then setIp =<< get b
                               else next i

eval1 i@JF    (a:b:_)   = do n <- get a
                             if n == 0
                               then setIp =<< get b
                               else next i

eval1 i@Sum   (a:b:c:_) = do set c =<< (+) <$> get a <*> get b
                             next i

eval1 i@Mul   (a:b:c:_) = do set c =<< (*) <$> get a <*> get b
                             next i

eval1 i@Less  (a:b:c:_) = do set c =<< fmap fromEnum ((<) <$> get a <*> get b)
                             next i

eval1 i@Equal (a:b:c:_) = do set c =<< fmap fromEnum ((==) <$> get a <*> get b)
                             next i

eval1   Halt  _         = error "eval1: Halt"

eval1   Unk   _         = error "eval1: Unk"

-- | Read a modal parameter
get :: Mode Int -> Op Int
get (Imm x)  = pure x
get (Pos px) = RWS.gets (fromMaybe 0 . (!? px) . _mem)
get (Rel rx) = RWS.gets _rb >>= \rb ->
                 RWS.gets (fromMaybe 0 . (!? (rx+rb)) . _mem)

-- | Write to a modal parameter
set :: Mode Int -> Int -> Op ()
set (Imm _) _ = error "setting to an immediate"
set (Pos x) n = RWS.modify (\m -> m { _mem = M.insert x n (_mem m) })
set (Rel x) n = RWS.gets _rb >>= \rb ->
                  RWS.modify (\m -> m { _mem = M.insert (x + rb) n (_mem m) })

adjustRelativeBase :: Int -> Op ()
adjustRelativeBase n = RWS.modify $ \m -> m { _rb = _rb m + n }

-- | Read the input
ask :: Op Int
ask = RWS.ask

-- | Increment the instruction pointer
incrIp :: Int -> Op ()
incrIp n = RWS.gets _ip >>= setIp . (n +)

-- | Set the instruction pointer
setIp :: Int -> Op ()
setIp n = RWS.modify (\m -> m { _ip = n })

-- | Advance the instruction pointer to the next instruction
next :: IntCode -> Op ()
next = incrIp . len
