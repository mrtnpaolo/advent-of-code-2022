module Main ( main ) where

import Advent    ( getInput )
import Data.List ( tails, sort, group )

main =
  do inp <- getInput id 6
     print (part1 inp)
     print (part2 inp)

part1 = solve 4

part2 = solve 14

solve n xs = head [ i | (i,ys) <- zip [n..] (take n <$> tails xs), uniq ys ]

uniq = all (null . tail) . group . sort
