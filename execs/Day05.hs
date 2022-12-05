module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.List          qualified as L
import Data.List.Split
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array         qualified as A
import Debug.Trace

main =
  do inp <- getInput parse 5
     putStrLn (part1 inp)
     putStrLn (part2 inp)
  where
    parse = p . splitOn "\n\n" . map clean
    clean c
      | c `elem` "[]" = ' '
      | isLower c     = ' '
      | otherwise     = c
    p [s,instrs] = (stacks s,map toI (lines instrs))

stacks xs = a
  where
    ys = map (filter (not . isSpace) . concat) . L.transpose $ map simpl $ init $ lines xs
    simpl = chunksOf 4
    a :: A.Array Int [Char]
    a = A.listArray (1,len) ys
    len = length ys

toI xs = let [n,from,to] = map (read @Int) (words xs) in (n,from,to)

go ss [] = ss
go ss ((n,from,to):rs) = go ss' rs
  where
    ss' = go' n from to ss

go' 0 _ _ ss = ss
go' n f t ss = go' (n-1) f t ss2
  where
    (x:xs) = ss A.! f
    ss1 = ss A.// [(f,xs)]
    ys = ss A.! t
    ss2 = ss1 A.// [(t,x:ys)]

part1 (ss,instrs) = map head $ A.elems $ go ss instrs

part2 (ss,instrs) = map head $ A.elems $ go2 ss instrs

go2 ss [] = ss
go2 ss ((n,from,to):rs) = go2 ss' rs
  where
    ss' = go2' n from to ss

go2' n f t ss = ss2
  where
    (ls,rs) = splitAt n (ss A.! f)
    ss1 = ss A.// [(f,rs)]
    ys = ss A.! t
    ss2 = ss1 A.// [(t,ls++ys)]
