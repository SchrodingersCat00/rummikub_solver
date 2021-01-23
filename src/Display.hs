{-# LANGUAGE OverloadedStrings #-}

module Display
    ( displaySolution
    ) where

import           Control.Monad (forM_)
import           Data.Function ((&))
import           Data.String   (IsString (fromString))
import           DataTypes     (Color (..), Rack, RumNum (..), Table, Tile (..),
                                TileSet, UniqueSets)
import           Rainbow       (Chunk, back, black, blue, fore, putChunk, red,
                                white, yellow)
import           Rainbow.Types (chunk)

-- |Converts a tile to a Chunk with color information
tileToChunk :: Tile RumNum Color -> Chunk
tileToChunk Joker = "J" & back white & fore black
tileToChunk (Tile n Black)  = fromString (show (fromEnum n + 1)) & back white  & fore black
tileToChunk (Tile n Red)    = fromString (show (fromEnum n + 1)) & back red    & fore white
tileToChunk (Tile n Orange) = fromString (show (fromEnum n + 1)) & back yellow & fore black
tileToChunk (Tile n Blue)   = fromString (show (fromEnum n + 1)) & back blue   & fore white

-- |Prints a color coded tile sequence to the terminal
displayTileSeq :: TileSet -> IO ()
displayTileSeq r = do
    forM_ r $ \t ->
        putChunk $ tileToChunk t

-- |Prepends a header string and appends a newline
displayHeader :: String -> IO () -> IO ()
displayHeader s m = do
    putStr $ s <> ": "
    m
    putStrLn ""

-- |Displays the color coded game state with solution
displaySolution :: Rack -> Table -> UniqueSets -> IO ()
displaySolution r t sol = do
    displayHeader "Rack" $ displayTileSeq r
    displayHeader "Table" $ displayTileSeq t
    displayHeader "Solution" $ do
        forM_ sol $ \seq -> do
            displayTileSeq seq
            putStr " "
        putStr $ "(" <> show ((sum . map length) sol - length t) <> " tiles placed)"
