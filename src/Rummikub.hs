module Rummikub
    ( main
    ) where

import qualified Data.List.Key as DLK
import Data.List (splitAt)

data Color
    = Red
    | Black
    | Blue
    | Orange
    deriving (Show)

type Tile = (Int, Color)
type SimpleTile = Int
type Board a = [[a]]
type Hand a = [a]

main :: IO ()
main = do
    print $ bestNextBoard [[1, 2, 3], [5, 6, 7]] [4, 6, 7, 5]

bestNextBoard :: (Num a, Eq a) => Board a -> Hand a -> Board a
bestNextBoard b h = (DLK.maximum l . filter f) (possibleBoards b h)
  where
    f = all (\x -> length x >= 3 && isSequence x)
    l = length . concat

isSequence :: (Eq a, Num a) => [a] -> Bool
isSequence [x, y] = x + 1 == y
isSequence (x:y:xs) = x + 1 == y && isSequence (y:xs)
isSequence _ = False

possibleBoards :: Board a -> Hand a -> [Board a]
possibleBoards b = foldl something (boardSplits b)

something :: [Board a] -> a -> [Board a]
something bs t = concatMap (`addTile` t) bs

addTile :: Board a -> a -> [Board a]
addTile [] t = []
addTile (s:ss) t = ((t:s) : ss):((s ++ [t]) : ss):[s:b | b <- addTile ss t]

boardSplits :: Board a -> [Board a]
boardSplits [] = [[]]
boardSplits l@(s:ss) = let brdsplts = boardSplits ss
                           sqsplts = seqSplits s
                       in [s:rs | rs <- brdsplts] ++ [f:s:rs | (f, s) <- sqsplts, rs <- brdsplts]

seqSplits :: [a] -> [([a], [a])]
seqSplits l = map (`splitAt` l) [1..length l - 1]
