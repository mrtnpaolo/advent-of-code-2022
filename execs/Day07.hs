module Main ( main ) where

import Advent                         ( getInput )
import Data.Maybe                     ( catMaybes )
import Data.List                      ( tails )
import Data.Map.Strict qualified as M ( fromListWith, elems )

main =
  do inp <- propagate <$> getInput parse 7
     print (part1 inp)
     print (part2 inp)

parse = walk [] . map words . lines

walk _ [] = []
walk cwd ([_,"cd",".."]:rest) =              walk (tail cwd) rest
walk cwd ([_,"cd",name]:rest) =              walk (name:cwd) rest
walk cwd ([_,"ls"     ]:rest) = (cwd,size) : walk cwd        rest'
  where
    (listing,rest') = span ((2==) . length) rest
    size = sum [ n | [file,_] <- listing, (n,_) <- reads @Int file ]

propagate paths = M.elems $ M.fromListWith (+)
  [ (parent,size) | (path,size) <- paths, parent <- tails path ]

part1 = sum . filter (<= 100_000)

part2 sizes = minimum [ size | size <- sizes, size >= needed ]
  where
    needed = 30_000_000 - (70_000_000 - head sizes)
