{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Either
import Data.List          qualified as L
import Data.List.Split    qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array qualified as A
import Data.Array.IArray ((!),(//))
import Debug.Trace

main =
  do inp <- getInput parse 11
     mapM_ print inp
     print (part1 inp)
     print (part2 inp)

parse = map (map (map (read @Int) . words) . lines) . L.splitOn "\n\n" . map \case c | not (isDigit c || isSpace c) -> ' '; c -> c

total = 8

monkeys = [0..total-1]

mkArr = A.listArray (0,total-1)

items = mkArr [[ 93, 98]
  ,[ 95, 72, 98, 82, 86]
  ,[ 85, 62, 82, 86, 70, 65, 83, 76]
  ,[ 86, 70, 71, 56]
  ,[ 77, 71, 86, 52, 81, 67]
  ,[ 89, 87, 60, 78, 54, 77, 98]
  ,[ 69, 65, 63]
  ,[ 89]
  ]

worries = mkArr [(*17),(+5),(+8),(+1),(+4),(*7),(+6),(^2)]

nexts = mkArr $ map next [ (19,5,3) ,(13,7,6) ,(5,3,0) ,(7,4,5) ,(17,1,6) ,(2,1,4) ,(3,7,2) ,(11,0,2) ]
  where
    next (p,t,f) n = dest
      where
        bored = n `mod` p == 0
        dest | bored = t | otherwise = f

{-

total = 4

monkeys = [0..total-1]

mkArr = A.listArray (0,total-1)
items = mkArr [[79, 98] ,[54, 65, 75, 74] ,[79, 60, 97] ,[74]]

worries = mkArr [(*19),(+6),(^2),(+3)]

nexts = mkArr $ map next [(23,2,3),(19,2,0),(13,1,3),(17,0,1)]
  where
    next (p,t,f) n = dest
      where
        bored = n `mod` p == 0
        dest | bored = t | otherwise = f
-}

part1 _ = product $ take 2 $ reverse $ L.sort $ A.elems a20
  where
    (_,a20) = last $ take 20 $ go items (mkArr (replicate total 0))

go items counts = (items',counts') : go items' counts'
  where
    (items',counts') = turn items counts

turn items counts = (items',counts')
  where
    (items',counts') = L.foldl' step (items,counts) monkeys

step (a,counts) i = (a'',counts')
  where
    candidates = a ! i
    worry = worries ! i
    next = nexts ! i
    updates =
      [ (i',[w])
      | n <- candidates
      , let w = (worry n) `div` 3
      , let i' = next w ]
    a' = a // [(i,[])]
    a'' = A.accum (++) a' updates
    counts' = counts // [(i,counts ! i + length candidates)]

part2 = const ()
