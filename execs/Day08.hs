module Main ( main ) where

import Advent ( count, getInputArray, above, right, below, left )

import Data.Array.Unboxed ( Ix( range, inRange ), amap, (!), bounds )

main =
  do inp <- getInputArray 8
     print (part1 inp)
     print (part2 inp)

part1 a = count (visible a) (range (bounds a))

visible a c@((a !) -> n) = any (all (< n)) (trees a c)

trees a c = [ [ a ! c' | c' <- cs ] | cs <- rays a c ]

rays a c = takeWhile (inside a) <$>
  [ tail (iterate dir c) | dir <- [above,right,below,left] ]

inside a = inRange (bounds a)

part2 a = maximum [ product (views a c) | c <- range (bounds a) ]

views a c@((a !) -> n) = [ view n ts | ts <- trees a c ]

view _ []     = 0 :: Int
view n (x:xs)
  | x >= n    = 1
  | x <  n    = 1 + view n xs
  | otherwise = 0
