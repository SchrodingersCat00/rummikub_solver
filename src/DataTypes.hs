{-# LANGUAGE ScopedTypeVariables #-}


module DataTypes
    ( Tile
    , Color
    , RumNum
    , getSets
    ) where

import qualified Data.Set as S
import Data.List (intercalate)
import Data.List.Split (splitOn)

data Color
    = Red
    | Black
    | Blue
    | Orange
    deriving ( Enum, Bounded, Show, Eq )

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
    deriving ( Enum, Bounded, Show, Eq )

data Tile a b -- Number Color
    = Tile a b
    | Joker
    deriving ( Show, Eq )

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

{-
Sets ordering (classic rummikub)

run,three,NJ   (44)
run,four,NJ    (40)
run,five,NJ    (36)
group,three,NJ (52)
group,four,NJ  (13)

run,three,1J   (92)
run,four,1J    (124)
run,five,1J    (148)
group,three,1J (78)
group,four,1J  (52)

run,three,2J   (52)
run,four,2J    (132)
run,five,2J    (233) -> (232?)
group,three,2J (0)
group,four,2J  (78)
-}

getSets :: IO [[Tile RumNum Color]]
getSets = do
    let t = [ embedColors $ n0J 3
            , embedColors $ n0J 4
            , embedColors $ n0J 5
            , g0J 3
            , g0J 4
            , embedColors $ n1J 3
            , embedColors $ n1J 4
            , embedColors $ n1J 5
            , g1J 3
            , g1J 4
            , embedColors $ n2J 3
            , embedColors $ n2J 4
            , embedColors $ n2J 5
            , g2J 3
            , g2J 4
            ]
    writeTFile (concat t)
    -- print $ map length t
    readTFile "out"

writeTFile :: [[(Char, Int)]] -> IO ()
writeTFile seqs = do
    writeFile "out" tString
  where
    tString = intercalate "\n" $ map (intercalate "," . map tileToString) seqs

readTFile :: String -> IO [[Tile RumNum Color]]
readTFile f = do
    l <- lines <$> readFile f
    return $ map parseLine l
  where
    parseLine l = map parseTile (splitOn "," l)
    parseTile t = let c = (getColor . head) t
                      n = read $ tail t :: Int
                  in if n == 0 then Joker else Tile (toEnum (n - 1) :: RumNum) c
    getColor 'a' = Black
    getColor 'b' = Red
    getColor 'c' = Blue
    getColor 'd' = Orange

tileToString :: (Char, Int) -> String
tileToString (c, i) = c:show i

g0J :: Int -> [[(Char, Int)]]
g0J l = [[(c, n) | c <- cs] | n <- [1..13], cs <- subsets l ['a', 'b', 'c', 'd']]

g1J :: Int -> [[(Char, Int)]]
g1J l = [[(c, n) | c <- cs] ++ [('a', 0)] | n <- [1..13], cs <- subsets (l-1) ['a', 'b', 'c', 'd']]

g2J :: Int -> [[(Char, Int)]]
g2J l
    | l == 3 = []
    | otherwise =  [[(c, n) | c <- cs] ++ [('a', 0), ('b', 0)] | n <- [1..13], cs <- subsets (l-2) ['a', 'b', 'c', 'd']]

n0J :: Int -> [[Int]]
n0J l = [[n+l | l <- [0..(l-1)]] | n <- [1..(14-l)]]

n1J :: Int -> [[Int]]
n1J l = removeDuplicates $ map removeDuplicates $ n1J' l 1 1 []

n2J :: Int -> [[Int]]
n2J l = removeDuplicates $ n2J' l 2 1 []

embedColors :: [[Int]] -> [[(Char, Int)]]
embedColors l = [[(c, r) | r <- s] | s <- l, c <- ['a', 'b', 'c', 'd']]

n1J' :: Int -> Int -> Int -> [[Int]] -> [[Int]]
n1J' l j n acc
    | n == 15 - l = acc
    | otherwise  = n1J' l j (n + 1) (map (0:) (subsets (l-j) [n..(n+(l-1))] )) ++ acc

n2J' :: Int -> Int -> Int -> [[Int]] -> [[Int]]
n2J' l j n acc
    | n == 15 - l = acc
    | otherwise  = n2J' l j (n + 1) (map ([0, 0]++) (subsets (l-j) [n..(n+(l-1))] )) ++ acc

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = S.toList . S.fromList

subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs
