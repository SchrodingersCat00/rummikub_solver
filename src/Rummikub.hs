module Rummikub
    ( main
    , lp
    , parseLPResult
    ) where


import           Control.Monad             (forM_, when)
import           Control.Monad.LPMonad     (equalTo, execLPM, setDirection,
                                            setObjective, setVarKind, varBds,
                                            varEq, varLeq)
import           Data.LinearProgram        (GLPOpts (..), MsgLev (..),
                                            glpSolveVars, mipDefaults)
import           Data.LinearProgram.Common (Direction (Max), LP, LinFunc,
                                            VarKind (IntVar), linCombination)
import           Data.List.Utils           (countElem)
import qualified Data.Map                  as M
import           DataTypes                 (Rack, RumTile, Table, TileSet,
                                            UniqueSets, getSets, parseTileSeq)
import qualified Display
import           System.Environment        (getArgs)

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

-- |Generate string names for tile variables
yVars :: [String]
yVars = ['y':show x | x <- [1..53]]

-- |Generate string names for set variables
xVars :: [String]
xVars = ['x':show x | x <- [1..1173]]

-- |Linear function to maximize
objFun :: LinFunc String Int
objFun = linCombination (zip (repeat 1) yVars)

-- |Describes constraints for the Rummikub problem
lp :: UniqueSets -> Rack -> Table -> LP String Int
lp sets r t = execLPM $ do
    setDirection Max
    setObjective objFun
    forM_ xVars $ \j ->
        do  varBds j 0 2
            setVarKind j IntVar

    forM_ (zip yVars [0..]) $ \(l, i) ->
        do  varBds l 0 2
            let r_i = countInSet r (toEnum i)
            -- in GLPK upper bound can't be the same as lower for some reason
            when (r_i == 0) (varEq l 0)
            when (r_i /= 0) (varLeq l r_i)
            setVarKind l IntVar
            equalTo (linCombination (constrUtil i)) (countInSet t (toEnum i))
  where
    -- |Creates first constraint for a given i
    constrUtil tid =
        zip [if x == tid then -1 else 0 | x <- [0..52]] yVars ++
        zip [ countInSet (sets!!sid) (toEnum tid) | sid <- [0..1172]] xVars

parseLPResult sets (_, Just (v, d)) = M.foldrWithKey (\k v l -> let (n, c) = parseLPVarString k in if v > 0 && c == 'x' then sets!!(n-1):l else l) [] d
  where
    parseLPVarString :: String -> (Int, Char)
    parseLPVarString s = let v = head s
                             n = (read . tail) s :: Int
                         in (n, v)

-- |Counts tile in set and raises error if amount > 2
countInSet :: TileSet -> RumTile -> Int
countInSet t tl =
    let c = countElem tl t in
        if c > 2 then
            error $ "Tile is more than twice on the table: " <> show tl
        else c

main :: IO ()
main = do
    uniqSets <- getSets
    args <- getArgs
    let [rack, table] = map parseTileSeq args
    let constr = lp uniqSets rack table
    let opts = mipDefaults { msgLev = MsgOff }
    result <- glpSolveVars opts constr
    let parsed = parseLPResult uniqSets result
    Display.displaySolution rack table parsed
