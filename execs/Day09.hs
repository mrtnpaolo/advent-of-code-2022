{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
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
  do inp <- getInputLines parse 9
     print inp
     print (part1 inp)
     print (part2 inp)
  where
    parse = (\[d:_,read @Int -> n] -> (d,n)) . words

part1 = S.size . go S.empty (C 0 0) (C 0 0)

go ts _ _ [] = ts
go ts t h ((_,0):nexts) = go ts t h nexts
go ts t h ((d,n):nexts) = go ts' t' h' ((d,n-1):nexts)
  where
    (t',h') = move t h d
    ts' = t' `S.insert` ts

move t h 'U' = follow t (above h)
move t h 'R' = follow t (right h)
move t h 'D' = follow t (below h)
move t h 'L' = follow t (left  h)

follow t h'
  | t == h' = (t,h')
  | t `elem` neighbors h' = (t,h')
  | otherwise = (t',h')
  where
    d = addCoord h' (scaleCoord (-1) t)
    t' = addCoord (step d) t

step (C (-2) (-2)) = C (-1) (-1)
step (C (-2) (-1)) = C (-1) (-1)
step (C (-2) ( 0)) = C (-1) ( 0)
step (C (-2) ( 1)) = C (-1) ( 1)
step (C (-2) ( 2)) = C (-1) ( 1)

step (C (-1) (-2)) = C (-1) (-1)
step (C ( 0) (-2)) = C ( 0) (-1)
step (C ( 1) (-2)) = C ( 1) (-1)

step (C (-1) ( 2)) = C (-1) ( 1)
step (C ( 0) ( 2)) = C ( 0) ( 1)
step (C ( 1) ( 2)) = C ( 1) ( 1)

step (C ( 2) (-2)) = C ( 1) (-1)
step (C ( 2) (-1)) = C ( 1) (-1)
step (C ( 2) ( 0)) = C ( 1) ( 0)
step (C ( 2) ( 1)) = C ( 1) ( 1)
step (C ( 2) ( 2)) = C ( 1) ( 1)

step c = error (show c)

part2 = S.size . snake (S.singleton origin) (replicate 10 origin)

snake ts _ [] = ts
snake ts xs ((_,0):nexts) = snake ts xs nexts
snake ts xs ((d,n):nexts) = snake ts' xs' ((d,n-1):nexts)
  where
    xs' = tug xs d
    ts' = last xs `S.insert` ts

tug :: [Coord] -> Char -> [Coord]
tug (x:xs) d = scanl hop (move' d x) xs

hop h t = t'
  where
    (t',_) = follow t h

move' 'U' = above
move' 'R' = right
move' 'D' = below
move' 'L' = left
