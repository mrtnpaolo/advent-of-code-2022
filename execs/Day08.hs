module Main ( main ) where

import Advent.List  ( count )
import Advent.Input ( getInputArray )
import Advent.Coord ( above, right, below, left )

import Data.Ix                           ( range, inRange )
import Data.Array.Unboxed qualified as A ( amap, (!), bounds )

main =
  do inp <- A.amap (read . pure) <$> getInputArray 8
     print (part1 inp)
     print (part2 inp)

part1 a = count (visible a) (range (A.bounds a))

visible a c@((a A.!) -> n) = any (all (< n)) (trees a c)

trees a c = [ [ a A.! c' | c' <- cs ] | cs <- rays a c ]

rays a c = takeWhile (inside a) <$>
  [ tail (iterate dir c) | dir <- [above,right,below,left] ]

inside a = inRange (A.bounds a)

part2 a = maximum [ product (views a c) | c <- range (A.bounds a) ]

views a c@((a A.!) -> n) = [ view n ts | ts <- trees a c ]

view :: Int -> [Int] -> Int
view _ []     = 0
view n (x:xs)
  | x >= n    = 1
  | x <  n    = 1 + view n xs
  | otherwise = 0
