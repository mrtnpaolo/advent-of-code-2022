module Main ( main ) where

import Advent             ( getInputArray, bfsOn, cardinal )
import Data.Char          ( ord )
import Data.Array.Unboxed ( assocs, bounds, (!), Ix(inRange) )

main =
  do a <- getInputArray 12
     print (solve 'S' a)
     print (solve 'a' a)

solve start a = steps
  where
    Just steps = lookup end $ bfsOn fst next starts

    starts = [ (c,0) | (c,x) <- assocs a, x == start ]
    [end]  = [ c | (c,'E') <- assocs a ]

    next (c,n) = [ (d,n+1) | d <- cardinal c, inside a d, allow (a!c) (a!d) ]

inside a c = inRange (bounds a) c

allow (\case 'S' -> 'a'; x -> x -> from)
      (\case 'E' -> 'z'; x -> x -> to  )
  = ord to <= ord from + 1
