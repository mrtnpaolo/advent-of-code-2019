{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-top-binds -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns, TypeSynonymInstances, FlexibleInstances #-}
module Main (main) where

import Advent
import Data.Char
import Data.Maybe
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Graph.Inductive

import Debug.Trace

main :: IO ()
main =
  do m1 <- parse <$> readFile "inputs/input18-test01"
     let g1 = build m1
     prettyPrint g1
     let ends1 = filter (M.null . fromKeys . snd . fromJust . lab g1) (nodes g1)
     putStrLn ("g1 has " ++ show (length ends1) ++ " ends: " ++ show ends1)
     mapM_ print [ spLength 0 n g1 | n <- ends1 ]

     m2 <- parse <$> readFile "inputs/input18-test02"
     let g2 = build m2
     prettyPrint g2
     let ends2 = filter (M.null . fromKeys . snd . fromJust . lab g2) (nodes g2)
     putStrLn ("g2 has " ++ show (length ends2) ++ " ends: " ++ show ends2)
     mapM_ print [ spLength 0 n g2 | n <- ends2 ]

     m3 <- parse <$> getRawInput 18
     let g3 = build m3
     prettyPrint g3
     let ends3 = filter (M.null . fromKeys . snd . fromJust . lab g3) (nodes g3)
     putStrLn ("g3 has " ++ show (length ends3) ++ " ends: " ++ show ends3)
     mapM_ print [ spLength 0 n g3 | n <- ends3 ]

     -- print (flood maze (entrance maze,keys maze))
     -- print sptest
     -- let g = build maze
     -- prettyPrint g
     -- print (spLength 0 9 g)

sptest = sp 1 4 g >>= (traverse (`L.lookup` nodes))
  where
    g = mkGraph nodes edges :: Gr Char Int
    nodes = [(1,'a'),(2,'b'),(3,'c'),(4,'d')] :: [LNode Char]
    edges = [(1,2,100),(1,3,1),(3,2,98),(3,4,200),(2,4,0)] :: [LEdge Int]



type GrNode = LNode Point
type GrEdge = LEdge Int

type SearchGraph = Gr Point Int

build :: Maze -> SearchGraph
build m = go (mkGraph [(0,begin)] []) [(0,begin)]
  where
    begin = (entrance m,keys m) :: Point

    go g [] = g
    go g frontier = go g' frontier'
      where
        (g',frontier') = L.foldl' go' (g,[]) frontier

        go' (g,acc) (l,p) = (g'',acc ++ [ (n,q) | (n,(q,_)) <- zip ids qds ])
          where
            qds = flood m p
            ids = newNodes (length qds) g

            g'  = L.foldl' (flip insNode) g  [ (n,q)   | (n,(q,_)) <- zip ids qds ]
            g'' = L.foldl' (flip insEdge) g' [ (l,n,d) | (n,(_,d)) <- zip ids qds ]




f :: Maze -> Gr Point Int
f m = g''
  where
    begin = (entrance m,keys m) :: Point

    g = mkGraph nodes edges :: Gr Point Int

    nodes :: [GrNode]
    nodes = [(0,begin)]

    edges :: [GrEdge]
    edges = []

    pds = flood m begin :: [(Point,Int)] -- reachable points and distances

    ids = newNodes (length pds) g

    g'  = L.foldl' (flip insNode) g  [ (n,p)   | (n,(p,_)) <- zip ids pds ]
    g'' = L.foldl' (flip insEdge) g' [ (0,n,d) | (n,(_,d)) <- zip ids pds ]



type Coord = (Int,Int)
type Tile = Char

newtype Keys = Keys { fromKeys :: M.Map Coord Char }

instance Show Keys where
  show = L.sort . M.elems . fromKeys

data Maze = Maze
  { entrance :: Coord
  , keys     :: Keys
  , doors    :: M.Map Coord Char
  , maze     :: S.Set Coord
  } -- deriving ()

parse :: String -> Maze
parse raw = Maze entrance (Keys keys) doors (S.fromList $ map fst maze)
  where
    tiles = [ ((x,y),t) | (y,xs) <- zip [0..] (lines raw), (x,t) <- zip [0..] xs ]
    [entrance] = [ p | (p,'@') <- maze ]
    keys  = M.fromList [ (p,c) | (p,c) <- filter (isLower . snd) maze ]
    doors = M.fromList [ (p,c) | (p,c) <- filter (isUpper . snd) maze ]
    maze  = [ (p,t) | (p,t) <- tiles, t /= '#']

type Point = (Coord,Keys) -- (position, missing keys)

flood :: Maze -> Point -> [(Point,Int)]
flood m@Maze{..} (start,startMissing) = go (S.singleton start) [start] 0
  where
    go seen frontier d
      | null frontier = pts
      | otherwise     = pts ++ go seen' frontier' (d+1)
      where
        candidates = [ q | p <- frontier, q <- adj m p
                         , q `S.notMember` seen
                         , q `M.notMember` doors || toLower (doors M.! q) `notElem` M.elems (fromKeys startMissing) ]
        (found,free) = L.partition (`M.member` (fromKeys startMissing)) candidates
        seen' = S.union seen (S.fromList candidates)
        frontier' = free
        pts = [ ((p,Keys $ M.delete p (fromKeys startMissing)),d+1) | p <- found ]


adj :: Maze -> Coord -> [Coord]
adj Maze{..} (x,y) = filter (`S.member` maze) cardinals
  where
    cardinals = [(x,y-1),(x+1,y),(x,y+1),(x-1,y)]
