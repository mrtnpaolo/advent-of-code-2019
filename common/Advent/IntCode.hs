{-# LANGUAGE BangPatterns, BlockArguments, LambdaCase #-}
module Advent.IntCode where

import Data.Bool (bool)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

run :: [Int] {- ^ Memory -} -> [Int] {- ^ Inputs -} -> [Int] {- ^ Outputs -}
run = outs . eval . load

run' :: [Int] -> Effect
run' = eval . load

outs :: Effect -> [Int] {- ^ Inputs -} -> [Int] {- ^ Outputs -}
outs (Output x eff) ins    = x : outs eff ins
outs (Input _)      []     = error "starving for input"
outs (Input f)      (x:xs) = outs (f x) xs
outs (Stop)         _      = []

data Machine = Machine
  { mem_     :: !(IntMap Int)
  , ip_      :: !(Int)
  , relBase_ :: !(Int) }

get :: Int {- ^ Position -} -> Machine -> Int
get i m = M.findWithDefault 0 i (mem_ m)

(!) :: Machine -> Int {- ^ Position -} -> Int
(!) = flip get

set :: Int {- ^ Position -}
    -> Int {- ^ New value -}
    -> Machine -> Machine
set i x m = m { mem_ = x `seq` M.insert i x (mem_ m) }

adjustRelBase :: Int -> Machine -> Machine
adjustRelBase x m = m { relBase_ = x + relBase_ m }

load :: [Int] {- ^ Program -} -> Machine
load raw = Machine
  { mem_     = M.fromAscList (zip [0..] raw)
  , ip_      = 0
  , relBase_ = 0 }

data Effect
  = Output Int Effect
  | Input (Int -> Effect)
  | Stop

eval :: Machine -> Effect
eval m =
  case step m of
    Step m'      -> eval m'
    StepOut x m' -> Output x (eval m')
    StepInp f    -> Input (eval . f)
    StepStop _   -> Stop

data Step
  = Step     !Machine
  | StepOut  !Int !Machine
  | StepInp  (Int -> Machine)
  | StepStop !Machine

step :: Machine -> Step
step m = next m
  where

    next =
      case (fetch m) of
        Add a b c -> Step . adv 4 . set c (val a + val b)
        Mul a b c -> Step . adv 4 . set c (val a * val b)
        Inp a     -> \m -> StepInp (\x -> adv 2 $ set a x m)
        Out a     -> StepOut (val a) . adv 2
        Jnz a b | 0 == val a -> Step . adv 3
                | otherwise  -> Step . jmp (val b)
        Jz  a b | 0 /= val a -> Step . adv 3
                | otherwise  -> Step . jmp (val b)
        Lt  a b c -> Step . adv 4 . set c (bool 0 1 $ val a < val b)
        Eq  a b c -> Step . adv 4 . set c (bool 0 1 $ val a == val b)
        Arb a     -> Step . adv 2 . adjustRelBase (val a)
        Hlt       -> StepStop . adv 1

    val i = m ! i

    adv n m = m { ip_ = n + ip_ m }
    jmp n m = m { ip_ = n         }

data IntCode
  = Add !Int !Int !Int -- ^ @c = a + b@
  | Mul !Int !Int !Int -- ^ @c = a * b@
  | Inp !Int           -- ^ @a = input()@
  | Out !Int           -- ^ @output(a)@
  | Jnz !Int !Int      -- ^ @if a then goto b@
  | Jz  !Int !Int      -- ^ @if !a then goto b@
  | Lt  !Int !Int !Int -- ^ @c = a < b@
  | Eq  !Int !Int !Int -- ^ @c = a == b@
  | Arb !Int           -- ^ @rel += a@
  | Hlt                -- ^ halt
  deriving Show

fetch :: Machine -> IntCode
fetch m = decoded
  where
    n = m ! (ip_ m)

    opcode = n `mod` 100

    decoded =
      case opcode of
        1  -> Add (par 1) (par 2) (par 3)
        2  -> Mul (par 1) (par 2) (par 3)
        3  -> Inp (par 1)
        4  -> Out (par 1)
        5  -> Jnz (par 1) (par 2)
        6  -> Jz  (par 1) (par 2)
        7  -> Lt  (par 1) (par 2) (par 3)
        8  -> Eq  (par 1) (par 2) (par 3)
        9  -> Arb (par 1)
        99 -> Hlt
        op -> error ("unknown opcode" ++ show op)

    par i =
      let a = ip_ m + i in
      case (mode i) of
        0 -> m ! a              -- positional
        1 ->     a              -- immediate
        2 -> m ! a + relBase_ m -- relative
        _ -> error "unsupported memory access mode"

    mode i = digit (i+1)

    digit k = (n `div` 10^k) `mod` 10
