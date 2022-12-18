module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Either
import Data.List          qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Debug.Trace

main =
  do inp <- getInputLines parse 18
     print (part1 inp)
     print (part2 inp)
  where
    parse = map (read @Int) . words . map \case ',' -> ' '; c -> c

part1 cubes = S.size $ (S.unions cubefaces) `S.difference` (S.fromList common)
  where
    cubefaces = map (S.fromList . faces) cubes
    common = [ f | (fs:fss) <- L.tails cubefaces, f <- S.toList fs, any (S.member f) fss ]

faces [x,y,z] =
  [ [[x,y,z],[x,y+1,z+1]]
  , [[x,y,z],[x+1,y,z+1]]
  , [[x,y,z],[x+1,y+1,z]]
  , [[x,y,z+1],[x+1,y+1,z+1]]
  , [[x,y+1,z],[x+1,y+1,z+1]]
  , [[x+1,y,z],[x+1,y+1,z+1]] ]

part2 cubes = sum outer
  where
    outer = [ n | p <- cubes, let n = count (`S.member` water) (faces p) ]

    water = S.fromList . concatMap faces $ bfs flood [xm,ym,zm]

    [xs,ys,zs] = L.transpose cubes

    (xm,xM) = (minimum xs - 1,maximum xs + 1)
    (ym,yM) = (minimum ys - 1,maximum ys + 1)
    (zm,zM) = (minimum zs - 1,maximum zs + 1)

    inside [x,y,z] = inRange ((xm,ym,zm),(xM,yM,zM)) (x,y,z)

    flood from = [ to | to <- around from, inside to, not (cube to) ]

    around [x,y,z] = [ [x+1,y,z], [x-1,y,z], [x,y+1,z], [x,y-1,z], [x,y,z+1], [x,y,z-1] ]

    cube p = S.member p (S.fromList cubes)
