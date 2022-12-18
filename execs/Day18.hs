module Main ( main ) where

import Advent                   ( getInputLines, bfs, count )
import Data.Ix                  ( inRange )
import Data.List                ( tails, transpose )
import Data.Set  qualified as S ( fromList, toList, unions, difference, member, size )

main =
  do inp <- getInputLines parse 18
     print (part1 inp)
     print (part2 inp)
  where
    parse = map (read @Int) . words . map \case ',' -> ' '; c -> c

part1 cubes = S.size $ (S.unions cubefaces) `S.difference` (S.fromList common)
  where
    cubefaces = map (S.fromList . faces) cubes
    common = [ f | (fs:fss) <- tails cubefaces, f <- S.toList fs, any (S.member f) fss ]

faces [x,y,z] =
  [ [[x,y,z],[x,y+1,z+1]]
  , [[x,y,z],[x+1,y,z+1]]
  , [[x,y,z],[x+1,y+1,z]]
  , [[x,y,z+1],[x+1,y+1,z+1]]
  , [[x,y+1,z],[x+1,y+1,z+1]]
  , [[x+1,y,z],[x+1,y+1,z+1]] ]

part2 cubes = sum outerfaces
  where
    outerfaces = [ count (`S.member` water) (faces c) | c <- cubes ]

    water = S.fromList . concatMap faces . bfs flood $ [xm,ym,zm]

    flood from = [ to | to <- around from, inside to, not (cube to) ]

    around [x,y,z] = [ [x+1,y,z], [x-1,y,z], [x,y+1,z], [x,y-1,z], [x,y,z+1], [x,y,z-1] ]

    [(xm,xM),(ym,yM),(zm,zM)] = map (\a -> (minimum a - 1,maximum a + 1)) (transpose cubes)

    inside [x,y,z] = inRange ( (xm,ym,zm), (xM,yM,zM) ) (x,y,z)

    cube p = S.member p (S.fromList cubes)
