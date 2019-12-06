module Main (main) where

import Advent
import Data.Char (ord)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Data.Tree
import Data.Graph
import Control.Arrow ((***))

main :: IO ()
main =
  do pairs <- map (toPair . words) . lines . map sep <$> getRawInput 6
     let bounds = (minimum *** maximum) $ unzip pairs
         g      = buildG bounds pairs
     -- putStrLn $ drawForest $ fmap (fmap show) $ dfs g [0]
     print $ part1 g 0
     print $ part2 g 1 2

  where
    sep ')' = ' '
    sep x = x

    toPair [ "COM" ,   ys ] = (       0 , hash ys )

    toPair [ "YOU" ,   ys ] = (       1 , hash ys )
    toPair [    xs ,"YOU" ] = ( hash xs , 1       )

    toPair [ "SAN" ,   ys ] = (       2 , hash ys )
    toPair [    xs ,"SAN" ] = ( hash xs , 2       )

    toPair [    xs ,   ys ] = ( hash xs , hash ys )

    hash [x]     = 3 + ord x
    hash [a,b,c] = ord a * 10000 + ord b * 100 + ord c


part1 :: Graph -> Vertex -> Int
part1 g name = sum . zipWith (\n xs -> n * length xs) [0..] . levels . head . dfs g $ [name]

part2 :: Graph -> Vertex -> Vertex -> Int
part2 g from to = abs (depth from - depth connection) + abs (depth to - depth connection) - 2
  where
    lvls = levels . head . dfs g $ [0]
    connection = head $ connections g [from,to]
    depth v = fromJust $ findIndex (elem v) lvls

-- | Filter the graph for vertices connecting the endpoints, depth-most first
connections :: Graph -> [Vertex] {- ^ endpoints -} -> [Vertex]
connections g ends = concat vs
  where
    lvls = levels $ head (dfs g [0])
    vs = filter (not . null) [ [ v | v <- lvl, all (path g v) ends ] | lvl <- reverse lvls ]
