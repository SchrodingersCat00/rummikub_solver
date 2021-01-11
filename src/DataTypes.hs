{-# LANGUAGE ScopedTypeVariables #-}

module DataTypes
    ( Tile
    , Color
    , RumNum
    , Set
    , main
    ) where

import qualified Data.Set as S

data Color
    = Red
    | Black
    | Blue
    | Orange
    deriving ( Enum, Bounded, Show )

data RumNum
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Eleven
    | Twelve
    | Thirteen
    deriving ( Enum, Bounded, Show )

data Tile a b -- Number Color
    = Tile a b
    | Joker
    deriving ( Show )

instance (Bounded a, Bounded b) => Bounded (Tile a b) where
    minBound = Tile minBound minBound
    maxBound = Joker

instance forall a b. (Enum a, Enum b, Bounded a, Bounded b) => Enum (Tile a b) where
    toEnum n
        | n == (fromEnum (maxBound :: a) + 1) * (fromEnum (maxBound :: b) + 1) = Joker
        | otherwise = Tile (toEnum (n `div` (fromEnum (maxBound :: b) + 1)))
                           (toEnum (n `mod` (fromEnum (maxBound :: b) + 1)))
    fromEnum (Tile x y) = fromEnum x * (fromEnum (maxBound :: b) + 1) + fromEnum y
    fromEnum Joker = (fromEnum (maxBound :: a) + 1) * (fromEnum (maxBound :: b) + 1)

newtype Set a b = Set [Tile a b]

instance (Bounded a, Bounded b, Enum a, Enum b) => Enum (Set a b) where
    toEnum n = Set []
    fromEnum (Set l) = 0

main :: IO ()
main = do
    -- print $ length ([[x, y, z] | z <- [minBound .. maxBound], y <- [minBound .. maxBound], x <- [minBound .. maxBound]] :: [[Tile RumNum Color]])
    let a = someFunc' 1 []
    let b = map S.fromList a
    print $ S.fromList b
    print $ length $ S.fromList b


someFunc :: Int -> [[Int]] -> [[Int]]
someFunc 12 acc = acc
someFunc n acc = someFunc (n + 1) (map (0:) (subsets 2 [n..(n+2)]))++acc

someFunc' :: Int -> [[Int]] -> [[Int]]
someFunc' 10 acc = acc
someFunc' n acc = someFunc' (n + 1) (map (0:) (subsets 4 [n..(n+4)]))++acc

subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs
