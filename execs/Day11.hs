module Main (main) where

import Prelude hiding (Either(..))

import Advent
import Advent.IntCode

import Data.Map (Map)
import qualified Data.Map as M

main :: IO ()
main =
  do mem <- map read . words . map sep <$> getRawInput 11
     print (part1 mem)
  where
    sep ',' = ' '
    sep x = x

-- black = 0
-- white = 1

type Coord = (Int,Int)
data Dir = Up | Right | Down | Left deriving (Show,Enum)

left :: Dir -> Dir
left = toEnum . (`mod`4) . pred . fromEnum

right :: Dir -> Dir
right = toEnum . (`mod`4) . succ . fromEnum

move :: Dir -> Coord -> Coord
move Up    (x,y) = (x,y+1)
move Right (x,y) = (x+1,y)
move Down  (x,y) = (x,y-1)
move Left  (x,y) = (x-1,y)

type Color = Int

part1 :: [Int] -> Int
part1 mem = M.size painted
  where
    painted = go (run' mem) 0 (0,0) Up (M.empty)

    look :: Coord -> Map Coord (Color,Int) -> Int
    look p = fst . M.findWithDefault (0,undefined) p

    go :: Effect
       -> Int           {- ^ 0 expecting color, 1 expecting direction -}
       -> Coord         {- ^ coordinate on the panels -}
       -> Dir           {- ^ bot direction -}
       -> Map Coord (Color,Int) {- ^ panels -}
       -> Map Coord (Color,Int) {- ^ panels' -}

    go Stop _ _ _ panels = panels

    go (Output color bot)     0 pos dir panels = go bot 1 pos dir panels'
      where
        panels' = M.alter paint pos panels
        paint Nothing           = Just (color,1)
        paint (Just (_,paints)) = Just (color,succ paints)

    go (Output direction bot) 1 pos dir panels = go bot 0 pos' dir' panels
      where
        dir' | direction == 0 = left dir
             | direction == 1 = right dir
        pos' = move dir' pos

    go (Input bot) n pos dir panels = go (bot $ look pos panels) n pos dir panels
