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

newtype Chem = Chem String deriving (Eq, Ord)
type Comp = (Int,Chem)
type Reaction = ([Comp],Comp)

main :: IO ()
main =
  do recs :: [Reaction] <- parse <$> getRawInput 14
     genEmojis recs
     print `mapM_` recs
     -- void $ M.traverseWithKey (\k v -> print (k,v)) (f recs)
     let depths = order recs
     print `mapM_` depths
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

