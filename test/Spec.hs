import Test.QuickCheck
import qualified Rummikub as R
import DataTypes ( Tile(..), RumNum(..), Color(..), getSets, createSets, parseTileSeq )
import Data.List.Utils (countElem)
import Data.Version(showVersion)
-- import Prelude hiding (Num(..))

import Control.Monad.LPMonad
    ( equalTo,
      execLPM,
      setDirection,
      setObjective,
      setVarKind,
      varBds,
      varEq,
      varLeq )
import Data.LinearProgram.Common
    ( Direction(Max), LP, linCombination, LinFunc, VarKind(IntVar) )
import Data.LinearProgram ( mipDefaults, glpSolveVars )
import qualified Data.Map as M
import Control.Monad ( forM_, when )
import qualified Data.Set as S
import Control.Monad ( forM_ )

type Rack = [Tile RumNum Color]
type Table = [Tile RumNum Color]
type UniqueSets = [[Tile RumNum Color]]

testData = [ ( parseTileSeq "j,k1,k2,j" -- rack
             , parseTileSeq "k1,k2,k3,k4"  -- table
             , [ [Tile One Black,Tile Two Black,Tile Three Black,Tile Four Black]
               , [Joker,Joker,Tile One Black,Tile Two Black]
               ]              --expected
             )
           , ( parseTileSeq "k3,k5,j,r1,o1"
             , parseTileSeq "k1,k2"
             , [ [Joker,Tile Two Black,Tile Three Black,Tile Five Black]
               , [Tile One Black,Tile One Red,Tile One Orange]
               ]
             )
           ]

main :: IO ()
main = do
    setsTest
    fullTests

setsTest = do
    let sets = createSets
    quickCheck (map length sets == [44,40,36,92,124,148,52,132,232,52,13,78,52,0,78])

fullTests = do
    sets <- getSets
    forM_ testData $ \(r, t, exp) -> do
        let constr = R.lp sets r t
        res <- glpSolveVars mipDefaults constr
        let parsed = R.parseLPResult sets res
        quickCheck (compareAsSet parsed exp)
        -- print parsed

compareAsSet :: (Eq a, Ord a) => [a] -> [a] -> Bool
compareAsSet x y = S.fromList x == S.fromList y