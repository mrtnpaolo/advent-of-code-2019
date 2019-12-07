module Main (main) where

import Advent

import Data.List (unfoldr, permutations)
import Data.Maybe (fromMaybe, listToMaybe)

import Data.IntMap.Strict (IntMap, (!?))
import qualified Data.IntMap.Strict as M

import Control.Monad.Trans.RWS.CPS (RWS)
import qualified Control.Monad.Trans.RWS.CPS as RWS

import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Debug.Trace

main :: IO ()
main =
  do mem <- fromInts . map read . words . map sep <$> getRawInput 7
     -- print $ part1 mem
     -- putStrLn "test 0: 9,8,7,6,5 -> 139629729"
     -- print $ feedback2 t0 [9,8,7,6,5]
     -- putStrLn "test 1: 9,7,8,5,6 -> 18216"
     -- print $ feedback2 t1 [9,7,8,5,6]
     -- print $ part2 t0
     -- print $ part2 t1
     print $ part2 mem
  where
    sep ',' = ' '
    sep x = x
    t0 = fromInts [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
    t1 = fromInts [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]

part1 :: Mem -> Int
part1 mem =
  maximum [ thrusters mem a b c d e | [a,b,c,d,e] <- permutations [0..4] ]

thrusters mem pa pb pc pd pe = e_o
  where [a_o] = run mem [pa,0]
        [b_o] = run mem [pb,a_o]
        [c_o] = run mem [pc,b_o]
        [d_o] = run mem [pd,c_o]
        [e_o] = run mem [pe,d_o]

part2 :: Mem -> Int
part2 mem =
  maximum [ last $ last $ feedback2 mem phases | phases <- permutations [5..9] ]

-- feedback mem pa pb pc pd pe = go ms 0
--   where
--     ms = load mem <$> S.fromList [[pa,0], [pb], [pc], [pd], [pe]]
--     go ms i =
--       case (run' m) of
--         ( Halt , _  , outs ) | i == length ms - 1 -> trace ("[m"++show i++"] halted " ++ show outs) $  outs
--                              | otherwise          -> go ms i'
--         ( Out  , m' , outs ) -> go (feed outs) i'
--       where
--         m = ms `S.index` i
--         i' = succ i `mod` length ms
--         m' = ms `S.index` i'
--         feed ins = S.adjust (\m -> m { _ins = _ins m ++ ins }) i' ms

data Effect
  = Output Int Effect
  | Input (Int -> Effect)
  | Stop

run'' :: Machine -> Effect
run'' m =
  case op of
    Out  -> let ( m', [out] ) = RWS.execRWS (exec op (ps @@ ms)) undefined m
            in Output out (run'' m')
    Inp  -> Input $ \n ->
              let m' = m { _ins = _ins m ++ [n] }
                  ( m'', outs ) = RWS.execRWS (exec op (ps @@ ms)) undefined m'
              in run'' m''
    Halt -> Stop
    _    -> let ( m' , _ ) = RWS.execRWS (exec op (ps @@ ms)) undefined m
            in run'' m'
  where
    ( (ip,(op,ms),ps), m', _ ) = RWS.runRWS fetch undefined m

effectuate :: Effect -> [Int] {- ^ Inputs -} -> [Int] {- ^ Outputs -}
effectuate Stop _ = []
effectuate (Output n e) ins = n : effectuate e ins
effectuate (Input _) [] = error "starving"
effectuate (Input f) (x:xs) = effectuate (f x) xs

feedback2 mem [pa,pb,pc,pd,pe] = outs
  where

    ms :: [Machine]
    ms = replicate 5 (load mem) -- <$> [[], [pb], [pc], [pd], [pe]]
    es :: [Effect]
    es = map run'' ms
    fs :: [ [Int] -> [Int] ]
    fs = map effectuate es
    -- outs :: [Int]
    outs = let oa = (fs!!0) (pa:0:oe)
               ob = (fs!!1) (pb:oa)
               oc = (fs!!2) (pc:ob)
               od = (fs!!3) (pd:oc)
               oe = (fs!!4) (pe:od)
           in [oa,ob,oc,od,oe]

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

load :: Mem -> Machine
load mem = Machine { _mem = mem, _ip = 0, _ins = [] }

-- | Run a machine up to its first output
run' :: Machine -> (IntCode,Machine,[Int]) {- ^ (Last opcode,Machine,Outputs) -}
run' m = RWS.runRWS go undefined m
  where
    go = do ~(ip,(opcode,modes),params) <- fetch
            case opcode of
              Out  -> exec opcode (params @@ modes) >> pure Out
              Halt ->                                  pure Halt
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

-- | IntCode machine state
data Machine = Machine { _mem :: Mem, _ip :: Int, _ins :: [Int] }

-- | IntCode machine operation
type OpEnv = ()
type OpLog = [Int]
type OpState = Machine
type Op = RWS OpEnv OpLog OpState

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

-- | 
-- ask' :: Op OpEnv
-- ask' = RWS.ask

-- | Peek at the next input, if available
peek :: Op (Maybe Int)
peek = RWS.gets (listToMaybe . _ins)

-- | Increment the instruction pointer
incrIp :: Int -> Op ()
incrIp n = RWS.gets _ip >>= setIp . (n +)

-- | Set the instruction pointer
setIp :: Int -> Op ()
setIp n = RWS.modify (\m -> m { _ip = n })

-- | Advance the instruction pointer to the next instruction
next :: IntCode -> Op ()
next = incrIp . len
