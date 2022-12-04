module Main ( main ) where

import Advent ( getInputLines, count )

main =
  do inp <- getInputLines parse 4
     print (part1 inp)
     print (part2 inp)

parse = map (read @Int) . words . map clean
  where
    clean = \case '-' -> ' '; ',' -> ' '; c -> c

part1 = count contained

contained [xl,xr,yl,yr] = (xl <= yl && yr <= xr) || (yl <= xl && xr <= yr)

part2 = count overlapped

overlapped [xl,xr,yl,yr] = not (yr < xl || yl > xr)
