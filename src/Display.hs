 {-# LANGUAGE OverloadedStrings #-}

module Display
    ( displaySolution
    ) where

import DataTypes
    ( Rack
    , Table
    , UniqueSets
    , Tile(..)
    , RumNum(..)
    , Color(..)
    )

import Rainbow
import Rainbow.Types (chunk)
import Data.Function ((&))
import Data.String ( IsString(fromString) )
import Control.Monad (forM_)

tileToChunk :: Tile RumNum Color -> Chunk
tileToChunk Joker = "J" & back white & fore black
tileToChunk (Tile n Black)  = fromString (show (fromEnum n + 1)) & back white  & fore black
tileToChunk (Tile n Red)    = fromString (show (fromEnum n + 1)) & back red    & fore white
tileToChunk (Tile n Orange) = fromString (show (fromEnum n + 1)) & back yellow & fore black
tileToChunk (Tile n Blue)   = fromString (show (fromEnum n + 1)) & back blue   & fore white

displayTileSeq :: Rack -> IO ()
displayTileSeq r = do
    forM_ r $ \t ->
        putChunk $ tileToChunk t

displayHeader :: String -> IO () -> IO ()
displayHeader s m = do
    putStr $ s <> ": "
    m
    putStrLn ""

displaySolution :: Rack -> Table -> UniqueSets -> IO ()
displaySolution r t sol = do
    displayHeader "Rack" $ displayTileSeq r
    displayHeader "Table" $ displayTileSeq t
    displayHeader "Solution" $ forM_ sol $ \seq -> do
        displayTileSeq seq
        putStr " "
