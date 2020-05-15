{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards, DeriveFunctor, DeriveFoldable #-}
module Main (main) where

import Advent
import qualified Data.List as L
import Data.List.Split
import Data.Foldable
import Data.Bifunctor
import Data.Ord (comparing)
import Data.Function (on)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Tree (Tree(..))
import qualified Data.Tree as T
import Debug.Trace

main :: IO ()
main =
  do parsed <- parse . reverse . lines . clean <$> getRawInput 14
     let amounts = M.fromList [ (name,amount) | (name,(amount,_)) <- parsed ]
     let rules   = M.fromList [ (name,rule) | (name,(_,rule)) <- parsed ]
     print amounts
     (\name ingredients -> putStr name >> putChar '\t' >> print ingredients) `M.traverseWithKey` rules
     let e = expr rules
     print e
     print (isSimple rules e)
     let s = simpl amounts e
     print s
     print (all (isSimple rules) s)
     print (part1 amounts rules s)
  where
    clean = map (overwrite ",=>" ' ')
      where
        overwrite these replacement c | c `elem` these = replacement
                                      | otherwise      = c
    parse = map (parseLine . chunksOf 2 . reverse . words)
    parseLine :: [[String]] -> (String,(Int,[(String,Int)]))
    parseLine ([chem,least]:srcs) = (chem,(read least,map (\[c,n] -> (c,read n)) (reverse srcs)))

part1 :: Amounts -> Rules -> [Expr String] -> Int
part1 amounts rules xs =
  sum [ n * ore
      | Expr name w [] <- xs
      , let least = amounts ! name
      , let Just n = L.findIndex (>= w) [ least * i | i <- [0..] ]
      , let [("ORE",ore)] = rules ! name
      ]

type Amounts = Map String Int

type Rule = (String,Int)
type Rules = Map String [Rule]

data Expr a = Expr a Int [Expr a]
  deriving (Show, Functor, Foldable)

expr :: Rules -> Expr String
expr rules = go "FUEL" 1
  where
    go name want =
      case (rules !? name) of
        Nothing -> Expr name want []
        Just recipe -> Expr name want [ go n w | (n,w) <- recipe, n /= "ORE" ]

isSimple :: Rules -> Expr String -> Bool
isSimple rules = all f
  where
    f name | [("ORE",_)] <- rules ! name = True | otherwise = False

simpl :: Amounts -> Expr String -> [Expr String]
simpl amounts = go
  where
    go :: Expr String -> [Expr String]
    go (Expr name want xs) = summands'
      where
        (simples,compounds) = L.partition isLeaf xs

        simplified = [ Expr baseName ((cap compoundName w)*w') xs
                     | compound@(Expr compoundName w _) <- compounds, Expr baseName w' xs <- go compound ]
          where
            cap name w = i
              where
                least = amounts ! name
                Just i = find (\i -> i * least >= w) [0..]

        summands = L.sortOn exprName (simples ++ simplified)
        groups = L.groupBy ((==) `on` exprName) summands
        summands' = map squish groups

        squish = L.foldl1' (\(Expr n w []) (Expr _ w' []) -> Expr n (w+w') [])
    -- go (Expr name want xs) = summands'
    --   where
    --     (leaves,branches) = L.partition isLeaf xs
    --     simplified = [ Expr n (w*w') xs | branch@(Expr _ w' _) <- branches, Expr n w xs <- go branch ]

    --     summands = L.sortOn exprName (leaves ++ simplified)
    --     groups = L.groupBy ((==) `on` exprName) summands
    --     summands' = map squish groups

    --     squish = L.foldl1' (\(Expr n w []) (Expr _ w' []) -> Expr n (w+w') [])

allLeaves :: [Expr a] -> Bool
allLeaves = all isLeaf

isLeaf :: Expr a -> Bool
isLeaf (Expr _ _ []) = True
isLeaf _ = False

exprName :: Expr String -> String
exprName (Expr name _ _) = name
