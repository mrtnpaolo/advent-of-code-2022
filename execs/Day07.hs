module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.List          qualified as L
import Data.List.Split    qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Data.Tree          qualified as T
import Debug.Trace

main =
  do inp <- getInputLines parse 7
     print inp
     print (part1 inp)
     print (part2 inp)

data Log = Cd String | Ls | Dir String | File String Int
  deriving (Show)

parse = f . words
  where
    f ["$","cd",dest] = Cd dest
    f ["$","ls"] = Ls
    f ["dir",name] = Dir name
    f [size,name] = File name (read size)

part1 = sum . filter (<= 100_000) . M.elems . luck . walk [] []

part2 = g . M.elems . luck . walk [] []

g xs = head . filter (>= needed) . L.sort $ xs
  where
    needed = 30_000_000 - (70_000_000 - head xs)

-- 30_000_000

luck = L.foldl' f M.empty

f m (k,[],n) = M.insert k n  m
f m (k,ks,n) = M.insert k (n + n') m
  where
    n' = sum $ map (m M.!) ks

walk _    fs []             = fs

walk path fs (Cd "..":rest) = walk (tail path) fs rest

walk path fs (Cd dest:Ls:rest) = walk (dest:path) fs' rest'
  where
    item = (tail . L.intercalate "/" . reverse $ dest:path,dirs,size)

    fs' = item : fs

    (contents,rest') = span notCd rest

    (dirNames,files) = L.partition isDir contents

    size = sum (map (snd . fromFile) files)

    dirs = map (tail . L.intercalate "/" . reverse . (:dest:path) . fromDir) dirNames

notCd (Cd _) = False
notCd _      = True

isDir (Dir _) = True
isDir (File _ _) = False
isDir _ = undefined

fromDir (Dir name) = name

fromFile (File name size) = (name,size)

