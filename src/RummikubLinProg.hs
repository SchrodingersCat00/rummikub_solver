module RummikubLinProg
    ( main
    ) where

import qualified DataTypes as DT
import Numeric.LinearProgramming

main :: IO ()
main = DT.main
-- main = do
--     let prob = Maximize [4, -3, 2]
--     let constr = Dense [ [2, 1, 0] :<=: 10
--                         , [0, 1, 5] :<=: 20
--                         ]
--     print $ simplex prob constr [ 2 :>=: 1, 3 :&: (2,7)]

{-  indices
    -------
    i = type of tile defined by color and number
    j = number of set (run or group)

    parameters
    ----------
    s_ij = indicates wheter a tile is in a set (0 or 1)
    t_i = tile i is 0, 1 or 2 times on the table
    r_i = tile i is 9, 1 or 2 times on your rack

    variables
    ---------
    x_j = set j can be placed 0, 1 or 2 times on the table
    y_i = tile i can be placed 0, 1 or 2 times from your rack on the table

    objective
    ---------
    maximize sum of y_i for all i [1..53]

    constraints
    -----------
    sum of all j's s_ij*x_j = t_i + y_i
    y_i <= r_i forall i
    x_j el {0, 1, 2}
    y_i el {0, 1, 2}

    Max sum of all tile y_i
-}