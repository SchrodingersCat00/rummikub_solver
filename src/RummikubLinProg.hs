module RummikubLinProg
    ( main
    ) where

import DataTypes
import Numeric.LinearProgramming
import Data.List.Utils (countElem)

type Rack = [Tile RumNum Color]
type Table = [Tile RumNum Color]
type UniqueSets = [[Tile RumNum Color]]

main :: IO ()
main = do
    -- probs = [y_1..y_53,x_1..x_1173]
    let prob = Maximize (replicate 53 1 ++ replicate 1173 0)
    -- let rack = map toEnum [12, 7, 11]
    -- let table = map toEnum [0, 4, 8, 1,
    -- let table = map toEnum [29, 5, 9, 2, 6, 10]
    -- let rack = map toEnum [1, 37, 8, 10, 14, 22, 23, 20, 50]
    -- let rack = map toEnum []
    -- let table = map toEnum [29, 30, 31, 44, 45, 46, 47, 32, 33, 34, 35, 39, 43, 47, 51, 40, 41, 42, 34, 38, 42]
    let rack = map toEnum [52, 0, 4, 52]
    let table = map toEnum [0, 4, 8, 12]
    print rack
    print table
    sets <- getSets
    -- print sets
    let constr = getConstraints sets rack table
    -- print $ sum $ map (tileInSet sets 8) [0..1172]
    let sol = simplex prob constr []
    print sol
    print $ parseLPResult sets sol

parseLPResult :: [[Tile RumNum Color]] -> Solution -> [(Int, [Tile RumNum Color])]
parseLPResult usets (Optimal (_, vars)) = map (\(i, _) -> (i, usets!!i)) $ filter (\(_, v) -> v == 1.0 || v == 2.0) (zip [0..1172] (drop 53 vars))
parseLPResult _ _ = error "Result not found"

tileInSet :: [[Tile RumNum Color]] -> Int -> Int -> Int
tileInSet ss tid sid
    | tile `elem` set = 1
    | otherwise       = 0
  where
    set = ss!!sid
    tile = toEnum tid :: Tile RumNum Color

getConstraints :: UniqueSets -> Rack -> Table -> Constraints
getConstraints s r t = Sparse (c1 ++ c2 ++ c3 ++ c4)
  where
    c1 = [((-1.0,i+1):[(fromIntegral (countInSet (s!!j) (toEnum i)), 53+j+1) | j <- [0..1172]]) :==: fromIntegral (countInSet t (toEnum i)) | i <- [0..52]]
    c2 = [[1#(i+1)] :<=: fromIntegral (countInSet r (toEnum i)) | i <- [0..52]]
    c3 = [[1#(53+j+1)] :&: (0, 2) | j <- [0..1172]]
    c4 = [[1#i] :&: (0, 2) | i <- [1..53]]

countInSet :: Table -> Tile RumNum Color -> Int
countInSet t tl = let c = countElem tl t in
                        if c > 2 then
                            error "tile is more than twice on the table"
                        else c

-- main = do
--     let prob = Maximize [4, -3, 2]
--     let constr = Dense [ [2, 1, 0] :<=: 10
--                         , [0, 1, 5] :<=: 20
--                         ]
--     print $ simplex prob constr [ 2 :>=: 1, 3 :&: (2,7)]

{-
    indices
    -------
    i = type of tile defined by color and number
    j = number of set (run or group)

    parameters
    ----------
    s_ij = indicates wheter a tile is in a set (0 or 1)
    t_i = tile i is 0, 1 or 2 times on the table
    r_i = tile i is 0, 1 or 2 times on your rack

    variables
    ---------
    x_j = set j can be placed 0, 1 or 2 times on the table
    y_i = tile i can be placed 0, 1 or 2 times from your rack on the table

    objective
    ---------
    maximize sum of y_i for all i [1..53]

    constraints
    -----------
    sum of all j's s_ij*x_j = t_i + y_i forall i
    => [0,...,-1,...,0,s_i1,...,s_i1173] = t_i forall i

    y_i <= r_i forall i
    => [0,...,1,...,0,0,...,0] <= r_i forall i

    x_j el {0, 1, 2} forall j
    => [0,...,0,0,...,1,...,0] el {0, 1, 2} forall j

    y_i el {0, 1, 2} forall i
    => [0,...,1,...,0,0,...,0] el {0, 1, 2} forall i

-}