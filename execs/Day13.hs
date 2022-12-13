module Main ( main ) where

import Advent      ( getInput )
import Data.List   ( sort )
import Text.Parsec ( parse, (<|>), many1, digit, sepBy, sepEndBy, char, between )

main =
  do inp <- getInput parse_ 13
     print (part1 inp)
     print (part2 inp)

parse_ xs = let Right ps = parse packets "" xs in ps

packets = pair `sepEndBy` char '\n'
pair    = packet `sepEndBy` char '\n'
packet  = PI <$> pnum <|> PL <$> plist
pnum    = read <$> many1 digit
plist   = between (char '[') (char ']') (packet `sepBy` (char ','))

data P = PI Int | PL [P]

pcmp (PI l) (PI r) | l < r  = LT | l == r = EQ | l > r  = GT

pcmp (PL [])     (PL [])     = EQ
pcmp (PL [])     _           = LT
pcmp _           (PL [])     = GT
pcmp (PL (l:ls)) (PL (r:rs)) = (pcmp l r) <> (pcmp (PL ls) (PL rs))

pcmp (PI l) (PL rs) = pcmp (PL [PI l]) (PL rs)
pcmp (PL ls) (PI r) = pcmp (PL ls) (PL [PI r])

part1 pkts = sum [ i | (i,LT) <- zip [1..] (map (\[l,r] -> pcmp l r) pkts) ]

instance Eq P  where l == r = EQ == pcmp l r

instance Ord P where compare = pcmp

part2 pkts = product
  [ i | (i,p) <- zip [1..] (sort (dividers ++ concat pkts)), p `elem` dividers ]

dividers = [PL [PL [PI 2]],PL [PL [PI 6]]]
