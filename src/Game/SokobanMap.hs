{-# LANGUAGE DeriveGeneric #-}

module Game.SokobanMap ( 
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
  { tileMapLocal :: [String],
    marksLocal :: [[Int]]
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

loadMapFromJSON :: FilePath -> Int -> IO (A.Array (Int, Int) Tile, (Int, Int), [(Int, Int)])
loadMapFromJSON path index = do
    putStrLn $ "Tentando carregar mapa do arquivo " ++ path ++ ", nível " ++ show index
    content <- B.readFile path
    case Ae.decode content of
        Just (MapWrapperLocal lvls) -> 
            if index < 0 || index >= length lvls then do
                putStrLn $ "Índice inválido: " ++ show index
                exitFailure
            else do
                let levelData = lvls !! index
                    rows = tileMapLocal levelData
                    marks = map (\[y, x] -> (y, x)) (marksLocal levelData)
                
                let (playerPos, mapList) = foldl
                        (\(accPos, accMap) (y, row) ->
                            let (posInRow, newRow) = foldl
                                    (\(p, r) (x, char) ->
                                        if char == '@'
                                            then ((y, x), r ++ [charToTile ' '])
                                            else (p, r ++ [charToTile char])
                                    ) (accPos, []) (zip [0..] row)
                            in (posInRow, accMap ++ [newRow])
                        ) ((-1, -1), []) (zip [0..] rows)

                let numRows = length mapList
                    numCols = length (head mapList)
                    assocs = [ ((y, x), mapList !! y !! x) | y <- [0..numRows - 1], x <- [0..numCols - 1] ]
                
                return (A.array ((0, 0), (numRows - 1, numCols - 1)) assocs, playerPos, marks)
        
        Nothing -> do
            putStrLn "Falha ao decodificar o JSON! Verifique o formato."
            putStrLn $ "Conteúdo bruto lido:\n" ++ show content
            exitFailure

