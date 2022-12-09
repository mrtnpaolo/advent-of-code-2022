module Main ( main ) where

import Advent ( getInput , Coord(..), origin, above, right, below, left, neighbors, addCoord )
import Data.Set qualified as S ( size, fromList )

main =
  do input <- getInput (concatMap parse . lines) 9
     print (part1 input)
     print (part2 input)

parse = (\[d:_,read @Int -> n] -> replicate n d) . words

part1 = trail 2

part2 = trail 10

trail n = S.size . S.fromList . map (!! pred n) . snake

snake = scanl step (repeat origin)

step (c:cs) d = scanl follow (move d c) cs

move 'U' = above
move 'R' = right
move 'D' = below
move 'L' = left

follow h@(C hy hx) t@(C ty tx)
  | t `elem` neighbors h = t
  | otherwise            = t `addCoord` C dy dx
  where
    dy = signum (hy - ty)
    dx = signum (hx - tx)
