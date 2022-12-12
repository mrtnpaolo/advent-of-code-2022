module Main ( main ) where

import Advent             ( getInputArray, bfsOn, cardinal )
import Data.Char          ( ord )
import Data.List          ( lookup )
import Data.Array.Unboxed ( assocs, bounds, (!), (//), Ix(inRange) )

main =
  do inp <- getInputArray 12
     print (part1 inp)
     print (part2 inp)

part1 arr = steps
  where
    Just steps = lookup end $ bfsOn repr next [(start,0)]

    [start] = [ c | (c,'S') <- assocs arr ]
    [end]   = [ c | (c,'E') <- assocs arr ]

    a = arr // [(start,'a'),(end,'z')]
    next (c,n) =
      [ (d,n+1) | d <- cardinal c, inside a d, ord (a!d) <= ord (a!c) + 1 ]

    repr (c,_) = c

part2 arr = steps
  where
    Just steps = lookup end $ bfsOn repr next [(c,0) | c <- starts]

    starts = [ c | (c,'a') <- assocs arr ]
    [end]  = [ c | (c,'E') <- assocs arr ]

    a = arr // [(end,'z')]
    next (c,n) =
      [ (d,n+1) | d <- cardinal c, inside a d, ord (a!d) <= ord (a!c) + 1 ]

    repr (c,_) = c

inside a c = inRange (bounds a) c
