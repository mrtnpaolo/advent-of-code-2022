module Main ( main ) where

import Advent          ( getInput )
import Data.Ord        ( Down(..) )
import Data.List       ( sortOn   )
import Data.List.Split ( splitOn  )

main =
  do nss <- getInput parse 1
     print (part1 nss)
     print (part2 nss)

parse = sortOn Down . map (sum . map (read @Int) . lines) . splitOn "\n\n"

part1 = head

part2 = sum . take 3
