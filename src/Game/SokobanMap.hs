{-# LANGUAGE DeriveGeneric #-}

module Game.SokobanMap ( 
    inBounds,
    loadMapFromJSON
) where

import qualified Data.Array as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as Ae
import Data.Aeson.Types (defaultOptions)
import System.Exit (exitFailure)
import GHC.Generics (Generic)
import Data.List (isSuffixOf)

import Game.Types 

data LevelLocal = LevelLocal
  { tileMapLocal :: [String]
  } deriving (Show, Generic)

data MapWrapperLocal = MapWrapperLocal
  { levelsLocal :: [LevelLocal]
  } deriving (Show, Generic)

dropSuffix :: String -> String -> String
dropSuffix suffix str
  | suffix `isSuffixOf` str = take (length str - length suffix) str
  | otherwise = str

instance Ae.FromJSON LevelLocal where
  parseJSON = Ae.genericParseJSON defaultOptions
    { Ae.fieldLabelModifier = dropSuffix "Local" }

instance Ae.FromJSON MapWrapperLocal where
  parseJSON = Ae.genericParseJSON defaultOptions
    { Ae.fieldLabelModifier = dropSuffix "Local" }

inBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inBounds ((minY, minX), (maxY, maxX)) (y, x) =
    x >= minX && x <= maxX && y >= minY && y <= maxY

loadMapFromJSON :: FilePath -> Int -> IO (A.Array (Int, Int) Tile)
loadMapFromJSON path index = do
    putStrLn $ "Tentando carregar mapa do arquivo " ++ path ++ ", nível " ++ show index
    content <- B.readFile path
    case Ae.decode content of
        Just (MapWrapperLocal lvls) -> 
            if index < 0 || index >= length lvls then do
                putStrLn $ "Índice inválido: " ++ show index
                exitFailure
            else do
                let rows = tileMapLocal (lvls !! index)
                    tileLists = map (map charToTile) rows
                    numRows = length tileLists
                    numCols = length (head tileLists)
                    assocs = [ ((y, x), tileLists !! y !! x) | y <- [0..numRows - 1], x <- [0..numCols - 1] ]
                return $ A.array ((0, 0), (numRows - 1, numCols - 1)) assocs
        Nothing -> do
            putStrLn "Falha ao decodificar o JSON! Verifique o formato."
            putStrLn $ "Conteúdo bruto lido:\n" ++ show content
            exitFailure

