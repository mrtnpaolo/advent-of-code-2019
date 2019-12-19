module Main (main) where

import Prelude hiding (Either(..))
import Advent
import Advent.IntCode
import Data.Foldable
import Control.Monad
import Debug.Trace

main :: IO ()
main =
  do mem <- getIntCode 19
     putStrLn (part1 mem 20 (8,14))
     -- putStrLn (part1 mem 20 (16,30))
     -- print . length . filter ('#'==) $ drawing

     let steps = ladder mem
     forM_ steps $ \(dir,(x,y),s) ->
       do print (dir,(x,y),s)
          let x' = case dir of S -> x; E -> x-s
              y' = case dir of S -> y-s; E -> y
              s' = case dir of S -> 2*s; E -> 2*s
          -- putStrLn (part1 mem s' (x',y))
          let found = contains mem 100 (x',y) (x'+s',y+s')
          when (not . null $ found) $
            do print found
               let ((x,y):_) = head found
               error $ show $ x*10000 + y

     -- forM_ [900..950] $ \y ->
     --   forM_ [600..620] $ \x ->
     --     do let found = contains mem 100 (x,y) (x+150,y+150)
     --        when (not $ null found) $
     --          do print (map (\(x,y) -> ((x,y),x*10000+y)) $ map head found)
     --             error "done"

contains mem size (xm,ym) (xM,yM) = [ coords | coords <- corners, all (act mem) coords ]
  where
    corners = [ [ (x,y), (x+size-1,y), (x,y+size-1) ] | x <- [xm..xM-size], y <- [ym..yM-size] ]

part1 mem size (xm,ym) = unlines $
  (flip map) [ym..ym+size-1] $ \y ->
    concat $ (flip map) [xm..xm+size-1] $ \x ->
      map f (run mem [x,y])
  where
    f 0 = '.'
    f 1 = '#'

data Dir = N | E | S | W deriving (Show)
move E (x,y) = (x+1,y)
move S (x,y) = (x,y+1)

ladder mem = takeWhile (\(_,_,s) -> s < 10000) $ go E (8,14)
  where
    go dir (x,y) = (dir,(nx,ny),s) : go dir' (nx,ny)
      where
        (nx,ny) = head $ dropWhile (act mem) $ dropWhile (not . act mem) $ iterate (move dir) (x,y)
        s = case dir of
          E -> nx-x
          S -> ny-y
        dir' = case dir of E -> S; S -> E

act mem (x,y) = 1 == (head $ run mem [x,y])


-- part2 mem size = go (2+4,4+8)
--   where

--     go (x,y)
--       | canFit = (top_xm + horiz_offset,y)
--       | otherwise = go (top_xm + horiz_offset,y+1)
--       where
--         top_xm = head [ x' | x' <- [x..], act (x',y) ]
--         bottom_y = y + size - 1
--         bottom_xm = head [ x' | x' <- [x..], act (x',bottom_y) ]
--         horiz_offset = bottom_xm - top_xm
--         xm = top_xm + horiz_offset
--         ym = y
--         xM = xm + size - 1
--         yM = ym + size - 1
--         canFit = all act [ (xm,ym), (xm,yM), (xM,ym), (xM,yM)]

    -- go (x,y)
    --   | y > 1000000 = error "escaped"
    --   | canFitHoriz && canFitVert = (x,y)
    --   | otherwise = traceShow ((x,y),xm,xM,xM-xm,xM-xm>=100) go (xm,y+1)
    --   where
    --     xm = head [ x' | x' <- [x..], act (x',y) ] -- find least active x on line y
    --     xs = takeWhile (\x -> act (x,y)) [xm..]
    --     xM = last xs

    --     canFitHoriz = act (xm+size-1,y)
    --     canFitVert  = act (xm,y+size-1)

    -- act (x,y) = 1 == (head $ run mem [x,y])
