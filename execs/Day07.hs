module Main ( main ) where

import Advent                  ( getInput )
import Data.Char               ( isDigit )
import Data.List               ( tails )
import Data.Map qualified as M ( fromListWith, elems )

main =
  do inp <- propagate . walk [] <$> getInput parse 7
     print (part1 inp)
     print (part2 inp)

parse = map words . filter relevant . lines
  where
    relevant (c:_) = c == '$' || isDigit c

walk _ [] = []
walk cwd ([_,"cd",".."]:next) =              walk (tail cwd) next
walk cwd ([_,"cd",name]:next) =              walk (name:cwd) next
walk cwd ([_,"ls"     ]:next) = (cwd,size) : walk cwd        next'
  where
    (files,next') = span (isDigit . head . head) next
    size          = sum (read @Int . head <$> files)

propagate paths = M.elems $ M.fromListWith (+)
  [ (parent,size) | (path,size) <- paths, parent <- tails path ]

part1 = sum . filter (<= 100_000)

part2 sizes = minimum [ size | size <- sizes, size >= needed ]
  where
    needed = 30_000_000 - (70_000_000 - head sizes)
