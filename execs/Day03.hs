module Main ( main ) where

import Advent                      ( getInputLines )
import Data.Char                   ( isUpper, toLower, ord )
import Data.List.Split             ( chunksOf )
import Data.IntSet qualified as IS ( fromList, intersection, toList )

main =
  do inp <- getInputLines parse 3
     print (part1 inp)
     print (part2 inp)

parse = map priority
  where
    priority c
      | isUpper c = 26 + priority (toLower c)
      | otherwise = ord c - ord 'a' + 1

part1 = sum . concat . map (uncurry uniq . halve)

halve xs = splitAt (length xs `div` 2) xs

uniq xs ys = IS.toList $ IS.fromList xs `IS.intersection` IS.fromList ys

part2 = sum . concat . map (foldr1 uniq) . chunksOf 3
