module SokobanMap ( 
    inBounds,
    loadMapFromJSON
) where

import qualified Data.Array as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as Ae
import System.Exit (exitFailure)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

import Types  

inBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inBounds ((minY, minX), (maxY, maxX)) (y, x) =
    x >= minX && x <= maxX && y >= minY && y <= maxY

loadMapFromJSON :: FilePath -> IO (A.Array (Int, Int) Tile)
loadMapFromJSON path = do
    putStrLn $ "Tentando carregar mapa de: " ++ path
    content <- B.readFile path
    case Ae.decode content of
        Just (MapWrapper rows) -> do
            let tileLists = map (map charToTile) rows
                numRows = length tileLists
                numCols = length (head tileLists)
                assocs = [ ((y, x), tileLists !! y !! x) | y <- [0..numRows - 1], x <- [0..numCols - 1] ]
            return $ A.array ((0, 0), (numRows - 1, numCols - 1)) assocs
        Nothing -> do
            putStrLn "Falha ao decodificar o JSON! Verifique o formato."
            putStrLn $ "Conteúdo bruto lido:\n" ++ show content
            exitFailure