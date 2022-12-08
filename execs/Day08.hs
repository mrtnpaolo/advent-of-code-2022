module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Either
import Data.List          qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Debug.Trace

main =
  do inp <- A.amap (read @Int . pure) <$> getInputArray 8
     print (part1 inp)
     print (part2 inp)

part1 a = (w+h)*2 +
  length [ ()
         | y <- ys, x <- xs
         , let n = a A.! (C y x)
         , let rs = rays bounds (C y x)
         , let vis = or [ and [ a A.! c' < n | c' <- r ] | r <- rs]
         , vis
         ]
  where
    bounds@(_,C h w) = A.bounds a
    ys = [1..h-1]
    xs = [1..w-1]

rays bounds c =
  [ takeWhile (inRange bounds) (tail $ iterate direction c)
  | direction <- [above,right,below,left] ]

part2 a = maximum
  [ product trees
  | y <- ys, x <- xs
  , let n = a A.! (C y x)
  , let rs = rays bounds (C y x)
  , let trees = [ view n [ a A.! c' | c' <- r ] | r <- rs ] ]
  where
    bounds@(_,C h w) = A.bounds a
    ys = [1..h-1]
    xs = [1..w-1]

view _ [] = 0 :: Int
view n (x:xs)
  | x >= n    = 1
  | x <  n    = 1 + view n xs
  | otherwise = 0
