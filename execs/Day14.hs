{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches -Wno-unused-top-binds -Wno-unused-local-binds #-}
{-# LANGUAGE ViewPatterns, TypeApplications, ScopedTypeVariables #-}
module Main (main) where

import Advent
import qualified Data.List as L
import Data.List.Split
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Control.Monad
import Data.Maybe
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace

newtype Chem = Chem String deriving (Eq, Ord)
type Comp = (Int,Chem)
type Reaction = ([Comp],Comp)
type Recipes = Map Chem (Int,[Comp])

main :: IO ()
main =
  do reacs :: [Reaction] <- parse <$> getRawInput 14
     genEmojis reacs
     putStrLn "reacs:"
     print `mapM_` reacs
     -- void $ M.traverseWithKey (\k v -> print (k,v)) (f reacs)

     let recips  = M.fromList [ (dst,(n,srcs)) | (srcs,(n,dst)) <- reacs ]
     putStrLn "recips:"
     void $ M.traverseWithKey (\k v -> print (k,v)) recips

     let depths = order reacs
     putStrLn "depths:"
     print `mapM_` depths

     let ordered = map fst depths
     print $ oreNeededFor recips ordered (1,Chem "FUEL")

     let f n = oreNeededFor recips ordered (n,Chem "FUEL") <= 1000000000000
     print $ binSearch f 1
  where
    parse = map (go id . words) . lines . map (\case c | c `elem` "=>," -> ' ' | otherwise -> c)
      where
        go srcs [read -> n, dst]         = (srcs [],(n,Chem dst))
        go srcs ((read -> n) : src : xs) = go (((n,Chem src):) . srcs) xs
        go _ _                           = undefined

order :: [Reaction] -> [(Chem,Int)]
order xs = L.sortOn (negate . snd) . M.toList $ steps
  where
    recipes = M.fromList [ (dst,(n,srcs)) | (srcs,(n,dst)) <- xs ]
    steps = depth <$> recipes
    depth (_,srcs) = maximum [ depthOf src | (_,src) <- srcs ] + 1
    depthOf x = M.findWithDefault (0::Int) x steps

oreNeededFor :: Recipes -> [Chem] -> Comp -> Int
oreNeededFor recips ordered (n,goalChem) = amounts ! (Chem "ORE")
  where
    amounts = L.foldl' go (M.singleton goalChem n) ordered
    go :: Map Chem Int -> Chem -> Map Chem Int
    go allNeeds chem = allNeeds' -- traceShow (chem,allNeeds') allNeeds'
      where
        allNeeds' = M.unionWith (+) newNeeds newNeeds'
        newNeeds  = M.delete chem allNeeds -- remove the current processed chem
        newNeeds' = M.fromList [ (neededChem,n*m) | (m,neededChem) <- directNeeds ]
        (amountMade,directNeeds) = recips M.! chem
        n = neededSoFar `divUp` amountMade
        neededSoFar = M.findWithDefault 0 chem allNeeds

divUp :: Integral a => a -> a -> a
x `divUp` y = (x + y - 1) `div` y

binSearch :: (Int -> Bool) -> Int -> Int
binSearch pred lo = search acceptable (2*acceptable)
  where
    acceptable = go lo
    go n | pred (2*n) = go (2*n)
         | otherwise  = n

    search lo hi
      | lo+1 == hi = lo
      | pred mid   = search mid hi
      | otherwise  = search lo mid
      where
        mid = lo + (hi - lo) `div` 2

-- hack to show recognizable images near each Chem when printed out

instance Show Chem where
  showsPrec _ (Chem xs) = showString xs . showString " " . showString (emoji xs)

emojis :: IORef (Map String String)
emojis = unsafePerformIO (newIORef M.empty)

emoji :: String -> String -- ❌ cross mark Unicode: U+274C
emoji xs = unsafePerformIO $ M.findWithDefault "\x274C" xs <$> readIORef emojis

genEmojis :: [Reaction] -> IO ()
genEmojis reacts =
  do writeIORef emojis (M.fromList assocs)
  where
    chems :: [String]
    chems = (\(srcs,(_,Chem dst)) -> dst : ((\(_,Chem src) -> src) <$> srcs)) =<< reacts
    predictable = "ORE" : "FUEL" : (L.nub chems L.\\ ["ORE","FUEL"])
    strs = map (:[]) ['\x1F34F'..] -- 🍏 green apple Unicode: U+1F34F
    assocs = zip predictable strs

