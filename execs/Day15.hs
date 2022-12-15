module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord hiding (clamp)
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
import Data.Bifunctor

main =
  do inp <- getInputLines parse 15
     print (part1 inp)
     print (part2 inp)
  where
    parse = f . map (read @Int) . words . map \case c | isDigit c -> c; '-' -> '-'; _ -> ' '
    f [sx,sy,bx,by] = (C sy sx,C by bx)

km = 0
kM = 4_000_000

part2 sbs = C y x -- [ C y x | y <- [0..k], x <- [0..k], inRow y, inCol x ]
  where
    rows = {- A.listArray (0,k) -} [ row sbs y | y <- [km..kM] ]
    cols = {- A.listArray (0,k) -} [ col sbs x | x <- [km..kM] ]
    [y] = [ y | y <- [km..kM], let r = minimize (row sbs y), not . null . tail $ r ]
    [x] = [ x | x <- [km..kM], let r = minimize (col sbs x), not . null . tail $ r ]

row sbs y =
  [ (sx-r+delta,sx+r-delta)
  | (s@(C sy sx),b) <- sbs
  , let r = manhattan s b
  , inRange (sy-r,sy+r) y
  , let delta = abs (sy-y) ]

col sbs x =
  [ (sy-r+delta,sy+r-delta)
  | (s@(C sy sx),b) <- sbs
  , let r = manhattan s b
  , inRange (sx-r,sx+r) x
  , let delta = abs (sx-x) ]


touches (xl,xr) (yl,yr) = not (yr < xl-1 || yl > xr+1)

minimize [] = []
minimize [r] = [r]
minimize (map clamp -> (r:rs))
  | null touching = r : minimize rs
  | otherwise = minimize (map (merge r) touching ++ disjunct)
  where
    (touching,disjunct) = L.partition (touches r) rs

merge (xl,xr) (yl,yr) = (min xl yl,max xr yr)

clamp (xl,xr) = (max km xl,min kM xr)


{-
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
-}

part1 xs = ()

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
