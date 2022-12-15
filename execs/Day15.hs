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
  do inp <- getInputLines parse 15
     print (part1 inp)
     print (part2 inp)
  where
    parse = f . map (read @Int) . words . map \case c | isDigit c -> c; '-' -> '-'; _ -> ' '
    f [sx,sy,bx,by] = (C sy sx,C by bx)


part1 xs = S.size no
  where
    k = 2_000_000

    f y (s@(C sy _),b) = inRange (sy - (d+1),sy + (d+1)) y
      where
        d = manhattan s b

    ys = filter (f k) xs

    area = S.unions
      [ S.fromList [ C k x | x <- ballrow s d k ] | (s,b) <- ys, let d = manhattan s b ]

    no = L.foldl' (flip S.delete) area [ b | (_,b) <- xs ]

ballrow s@(C sy sx) r y = range (sx-r+delta,sx+r-delta)
  where
    delta = abs (sy - y)

{-
part1 xs = ys M.! 2_000_000
  where
    fs =
      [ \c -> manhattan c s <= d && c /= b | (s@(C sy sx),b) <- xs, let d = manhattan s b ]
    ys = M.unionsWith (++) $
      [ M.fromList [ (y,[s]) | y <- [sy - (d+1) .. sy + (d+1) ] ]
      | (s@(C sy _),b) <- xs
      , let d = manhattan s b ]
-}

apply 0 _ x = x
apply n f x = apply (n-1) f (f x)

{-
part1 xs = S.size $ S.filter (\(C y _) -> y == 10) no
  where
    area = S.unions [ S.fromList $ ball s d | (s,b) <- xs, let d = manhattan s b ]
    no = L.foldl' (flip S.delete) area $ concat [ [s,b] | (s,b) <- xs ]
-}

ball c@(C y x) r = [ d | d <- range (C (y-r) (x-r),C (y+r) (x+r)), manhattan c d <= r ]

part2 = const ()
