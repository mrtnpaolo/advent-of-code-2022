module Main ( main ) where

import Advent ( getInputLines )
import Data.List       qualified as L ( scanl' )
import Data.List.Split qualified as L ( chunksOf )

main =
  do inp <- getInputLines (parse . words) 10
     print (part1 inp)
     putStr (part2 inp)

data Instruction = NoOp | AddX Int

parse ["noop"]   = NoOp
parse ["addx",n] = AddX (read n)

part1 inp = sum [ c*x | (c,x) <- concat (L.scanl' eval [(1,1)] inp), interesting c ]

eval ((c,x):_) NoOp     = [(c+1,x)]
eval ((c,x):_) (AddX n) = [(c+2,x+n),(c+1,x)]

interesting c = c `elem` [20,60,100,140,180,220]

part2 = unlines . L.chunksOf 40 . draw 1 1

draw _ _ []           = []
draw c x (NoOp  :ops) = hit c x : draw (c+1) x ops
draw c x (AddX n:ops) = hit c x : hit (c+1) x : draw (c+2) (x+n) ops

hit c x | ((c-1) `mod` 40) `elem` [x-1,x,x+1] = '#' | otherwise = ' '
