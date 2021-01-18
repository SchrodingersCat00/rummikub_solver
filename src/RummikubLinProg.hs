module RummikubLinProg
    ( main
    ) where

import DataTypes
import Data.List.Utils (countElem)
-- import Prelude hiding (Num(..))

import Control.Monad.LPMonad
import Data.LinearProgram.Common
import Data.LinearProgram
import qualified Data.Map as M
import Control.Monad ( forM_, when )

type Rack = [Tile RumNum Color]
type Table = [Tile RumNum Color]
type UniqueSets = [[Tile RumNum Color]]

yVars :: [String]
yVars = ['y':show x | x <- [1..53]]

xVars :: [String]
xVars = ['x':show x | x <- [1..1173]]

objFun :: LinFunc String Int
objFun = linCombination (zip (repeat 1) yVars)
-- objFun = linCombination [(10, "x1"), (6, "x2"), (4, "x3")]

lp :: UniqueSets -> Rack -> Table -> LP String Int
lp sets r t = execLPM $ do
    setDirection Max
    setObjective objFun
    forM_ xVars $ \x ->
        do  varBds x 0 2
            setVarKind x IntVar

    forM_ (zip yVars [0..]) $ \(l, y) ->
        do  varBds l 0 2
            let r_i = countInSet r (toEnum y)
            -- upper bound can't be the same as lower for some reason
            when (r_i == 0) (varEq l 0)
            when (r_i /= 0) (varLeq l r_i)
            setVarKind l IntVar
            equalTo (linCombination (someHelp sets y)) (countInSet t (toEnum y))

someHelp :: UniqueSets -> Int -> [(Int, String)]
someHelp sets tid = zip [if x == tid then -1 else 0 | x <- [0..52]] yVars ++ zip [ countInSet (sets!!sid) (toEnum tid) | sid <- [0..1172]] xVars

main :: IO ()
main = do
    -- probs = [y_1..y_53,x_1..x_1173]
    -- let prob = Maximize (replicate 53 1 ++ replicate 1173 0)
    -- let rack = map toEnum [12, 7, 11]
    -- let rack = map toEnum []
    -- let table = map toEnum [29, 30, 31, 44, 45, 46, 47, 32, 33, 34, 35, 39, 43, 47, 51, 40, 41, 42, 34, 38, 42]
    sets <- getSets
    -- let table = map toEnum [0, 4, 8, 1,
    -- let table = map toEnum [29, 5, 9, 2, 6, 10]
    -- let rack = map toEnum [1, 37, 8, 10, 14, 22, 23, 20, 50]
    let rack = map toEnum [52, 0, 4, 52]
    let table = map toEnum [0, 4, 8, 12]
    print rack
    print table
    let constr = lp sets rack table
    -- print constr
    result <- glpSolveVars mipDefaults constr
    -- print sets
    print $ parseLPResult result

parseLPResult (_, Just (v, d)) = M.foldrWithKey (\k v l -> if v > 0 then k:l else l) [] d

countInSet :: Table -> Tile RumNum Color -> Int
countInSet t tl = let c = countElem tl t in
                        if c > 2 then
                            error "tile is more than twice on the table"
                        else c

{-
    source: https://www.semanticscholar.org/paper/Solving-Rummikub-Problems-by-Integer-Linear-Hertog-Hulshof/ab00c1bfe35e21a2edb7287234af03f74a3ee3ae

    LP: specification
    indices
    -------
    i = type of tile defined by color and number
    j = number of set (run or group)

    parameters
    ----------
    s_ij = tile i appears 0, 1 or 2 times in set j
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
    s_i1*x_1 + ... + s_i1173*x1173 = t_i + y_i forall i
    y_i <= r_i forall i
    x_j el {0, 1, 2} forall j
    y_i el {0, 1, 2} forall i
-}