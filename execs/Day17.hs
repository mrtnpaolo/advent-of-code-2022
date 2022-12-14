module Main ( main ) where

import Advent ( getInputLines, Coord(..), coordRow, coordCol, addCoord
              , origin, cardinal, above, right, below, left, drawCoords )
import Data.Set        qualified as S
import Data.Map.Strict qualified as M
import Control.Applicative ((<|>))

main =
  do inp <- head <$> getInputLines id 17
     print (part1 inp)
     print (part2 inp)

type Cave = S.Set Coord
type Rock = S.Set Coord

part1 gas = height . (!! 2022) $ iterate fall (S.empty,cycle rocks,cycle gas)

rocks =
  [ S.fromList . take 4 . iterate right $ origin
  , S.map (addCoord (C (-1) 1)) $ S.fromList (origin : cardinal origin)
  , S.fromList [origin, right origin, right (right origin), above (right (right origin)), above (above (right (right (origin))))]
  , S.fromList [origin, above origin, above (above origin), above (above (above origin))]
  , S.fromList [origin, above origin, right (above origin), right origin] ]

fall :: (Cave,[Rock],String) -> (Cave,[Rock],String)
fall (cave,r:rs,gas) = (cave',rs,gas')
  where
    rock = place cave r
    cave' = S.union cave rock'
    (rock',gas') = go (rock,gas)
    go (rock,g:gs)
      | Just rock'' <- lower cave rock' = go (rock'',gs)
      | otherwise                       = (rock',gs)
      where
        rock' = push cave rock g

place cave rock = S.map (addCoord offset) rock
  where
    C start_y _ | S.null cave = (C 1 0) | otherwise = minimum cave
    offset = (C start_y 0) `addCoord` (C (-4) 2)

push :: Cave -> Rock -> Char -> Rock
push cave rock g
  | hit       = rock
  | otherwise = rock'
  where
    rock' = S.map (dx g) rock
    dx '<' = left
    dx '>' = right
    dx c = error (show c)
    hit = or [ rockHit, wallHit ]
    rockHit = not . S.null . S.intersection cave $ rock'
    wallHit = any ((\x -> x < 0 || 6 < x) . coordCol) rock'

lower :: Cave -> Rock -> Maybe Rock
lower cave rock
  | hit       = Nothing
  | otherwise = Just rock'
  where
    rock' = S.map below rock
    hit = or [ rockHit, bottomHit ]
    rockHit = not . S.null . S.intersection cave $ rock'
    bottomHit = any ((1==) . coordRow) rock'

part2 gas = height (frames !! (j+extra)) + cycleHeight * (cycleCount - 1)
  where
    frames = iterate fall (S.empty,cycle rocks,cycle gas)
    sigs = map (sig (length gas)) frames
    (i,j) = findCycle sigs

    cycleLen = j-i
    (cycleCount,extra) = (1_000_000_000_000 - i) `divMod` cycleLen
    cycleHeight = height (frames !! j) - height (frames !! i)

findCycle :: Ord a => [a] -> (Int,Int)
findCycle = go M.empty (0::Int)
  where
    go _ _ [] = error "no cycle"
    go seen i (x:xs)
      | Just j <- seen M.!? x = (j,i)
      | otherwise = go (M.insert x i seen) (i+1) xs

sig n (cave,rocks,gas) = ( S.map (addCoord (C (-ym) 0)) top, take 5 rocks, take n gas )
  where
    k = 100
    C ym _ = minimum cave
    (top,_) = S.split (C (ym+k) 0) cave

height = succ . negate . coordRow . minimum . (\(c,_,_) -> c)

showCave cave = drawCoords m
  where
    m = M.unions [ fixed, space, walls ]
    fixed = M.fromSet (const '#') cave
    Just (C ym _) = S.lookupMin cave <|> Just (C (-3) 0)
    walls = M.fromList [ (c,'???') | c <- concat [l,b,r] ]
    l = [ C y (-1) | y <- [ ym .. 0 ] ]
    r = [ C y 7    | y <- [ ym .. 0 ] ]
    b = [ C 1 x    | x <- [ -1 .. 7 ] ]
    space = M.fromList [ (c,'.') | c <- [ C y x | y <- [ ym .. 0 ], x <- [ 0 .. 6 ] ] ]
