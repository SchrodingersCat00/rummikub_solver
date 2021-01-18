import Test.QuickCheck
import qualified Rummikub as R
import DataTypes ( Tile(..), RumNum(..), Color(..), getSets )
import Data.List.Utils (countElem)
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

testData = [ ( [52, 0, 4, 52] -- rack
             , [0, 4, 8, 12]  -- table
             , [ [Tile One Red,Tile Two Red,Tile Three Red,Tile Four Red]
               , [Joker,Joker,Tile One Red,Tile Two Red]
               ]              --expected
             )
           , ( [8, 16, 52, 1, 2]
             , [0, 4]
             , [ [Joker,Tile Two Red,Tile Three Red,Tile Five Red]
               , [Tile One Black,Tile One Red,Tile One Blue]
               ]
             )
           ]

main :: IO ()
main = do
    sets <- getSets
    forM_ testData $ \(r, t, exp) -> do
        let r' = map toEnum r
        let t' = map toEnum t
        let constr = R.lp sets r' t'
        res <- glpSolveVars mipDefaults constr
        let parsed = R.parseLPResult sets res
        quickCheck (S.fromList parsed == S.fromList exp)
