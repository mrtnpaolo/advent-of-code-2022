module Main ( main ) where

import Advent     ( getInputLines, count, Coord(..), below, left, right )
import Data.Ix    ( range )
import Data.Maybe ( listToMaybe )
import Data.List  ( unfoldr, foldl' )
import Data.Map.Strict qualified as M
  ( unions, keys, elems, empty, union, fromList, notMember, insert )

main =
  do rocks <- M.unions <$> getInputLines parse 14
     let floor = 2 + maximum [ y | C y _ <- M.keys rocks ]
     print (part1 floor rocks)
     print (part2 floor rocks)
  where
    parse = path . words . map \case c | c `elem` ",->" -> ' '; c -> c
    path = foldl' line M.empty . (\xs -> zip xs (tail xs)) . split
      where
        split (x:y:xs) = C (read y) (read x) : if null xs then [] else split xs
        line m (a,b) = M.union m $ M.fromList [ (c,'#') | c <- range bounds ]
          where
            bounds = (min a b,max a b)

begin = C 0 500

pour m from = unfoldr next from
  where
    next c = listToMaybe [ (d,d) | d <- dests c, d `M.notMember` m ]
    dests c = [below c,left (below c),right (below c)]

part1 floor m = go m
  where
    go m
      | any (>= floor) [ y | C y _ <- path ] = count ('o'==) (M.elems m)
      | otherwise = go (M.insert dest 'o' m)
      where
        path = pour m begin
        dest = last path

part2 floor m = go m (pourOnTheFloor m begin)
  where
    go m path@(dest:(~(next:rest)))
      | path == [begin] = count ('o'==) (M.elems $ M.insert begin 'o' m)
      | otherwise = go m' (pourOnTheFloor m' next ++ rest)
      where
        m' = M.insert dest 'o' m

    pourOnTheFloor m from = reverse path
      where
        path = from : takeWhile (\(C y _) -> y < floor) (pour m from)
