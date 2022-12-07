module Main (main) where

import Advent                         ( getInput )
import Data.Char                      ( isSpace, isDigit )
import Data.List       qualified as L ( transpose, foldl', splitAt )
import Data.List.Split                ( splitOn )
import Data.Array      qualified as A ( listArray, (!), (//), elems )

main =
  do inp <- getInput parse 5
     putStrLn (uncurry part1 inp)
     putStrLn (uncurry part2 inp)

parse contents = (stacks,instrs)
  where
    [raw_stacks,raw_instrs] = splitOn "\n\n" contents
    stacks =
      (\xs -> A.listArray (1,length xs) xs) .
      map (dropWhile isSpace . reverse . tail) . -- ["NZ","DCM,"P"]
      filter (isDigit . head) .                  -- ["1NZ ","2DCM","3P  "]
      L.transpose . reverse . lines $ raw_stacks
    instrs =
      map (map (read @Int) . words) .            -- [[1,2,1],...]
      map (map (\case c | isDigit c -> c | otherwise -> ' ')) .
      lines $ raw_instrs

part1 = solve reverse

part2 = solve id

solve f stacks instrs =
  map head . A.elems $ L.foldl' (go f) stacks instrs

go f stacks [n,from,to] = stacks A.// [(from,xs'),(to,f zs<>ys)]
  where
    xs = stacks A.! from
    ys = stacks A.! to
    (zs,xs') = L.splitAt n xs
