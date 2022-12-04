module Main ( main ) where

import Advent ( getInputLines )

data RPS = R | P | S -- Rock Paper Scissors
  deriving (Show,Ord,Eq,Enum)

main =
  do inp <- getInputLines parse 2
     print (part1 inp)
     print (part2 inp)
  where
    parse [opp,' ',me] = (p1 me, p2 opp)
      where
        p1 = \case 'X' -> R; 'Y' -> P; 'Z' -> S
        p2 = \case 'A' -> R; 'B' -> P; 'C' -> S

score :: RPS -> Int
score = succ . fromEnum

out :: RPS {- own choice -}
    -> RPS {- opponent's -}
    -> Int {- outcome    -}

out R S = 6
out P R = 6
out S P = 6

out R R = 3
out P P = 3
out S S = 3

out _ _ = 0

part1 = sum . map (\(me,opp) -> score me + out me opp)

type Outcome = RPS -- R lose, P draw, S win

f :: Outcome {- choice to interpret as outcome -}
  -> RPS     {- opponent's choice              -}
  -> RPS     {- matching choice to be made     -}

f R R = S
f R P = R
f R S = P

f P R = R
f P P = P
f P S = S

f S R = P
f S P = S
f S S = R

part2 = part1 . map (\(me,opp) -> (f me opp,opp))

-- curiously f is commutative: f me opp = f opp me
--
--   R P S
-- R S R P
-- P R P S
-- S P S R
--
-- the matrix is symmetric
