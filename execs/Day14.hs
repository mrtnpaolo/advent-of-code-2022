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
  do inp <- M.unions . (M.singleton begin '+' :) <$> getInputLines parse 14
     putStrLn (drawCoords inp)
     print (part1 inp)
     print (part2 inp)
  where
    parse = path . words . map \case c | c `elem` ",->" -> ' '; c -> c
    path = L.foldl' go M.empty . (\xs -> zip xs (tail xs)) . split
      where
        split [] = []
        split (x:y:xs) = C (read y) (read x) : split xs
        go m (a,b) = M.union m $ M.fromList [ (c,'#') | c <- range bounds ]
          where
            bounds = (min a b,max a b)

begin = C 0 500

pour m = path m begin
  where
    ybyss = 1 + maximum [ y | (C y x,'#') <- M.toList m ]
    path m c@(C y x)
      | Just d <- listToMaybe [ d | d <- dests c, free m d ] =
        if y == ybyss then [Left d] else Right d : path (M.insert d '.' m) d
      | otherwise = []

dests c = [below c,left (below c),right (below c)]

free m c = M.notMember c m

part1 = go
  where
    go m
      | null (lefts path) = go (M.insert d 'o' m)
      | otherwise = count ('o'==) [ x | (c,x) <- M.toList m ]
      where
        path = pour m
        Right d = last path

part2 = const ()
