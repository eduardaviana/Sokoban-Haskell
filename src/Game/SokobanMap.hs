-- | Habilita a extensão 'DeriveGeneric' para a derivação automática de instâncias de 'FromJSON'.
{-# LANGUAGE DeriveGeneric #-}

-- | Módulo que lida com a estrutura e o carregamento de mapas para o jogo
module Game.SokobanMap where

import qualified Data.Array as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as Ae
import Data.Aeson.Types ()
import System.Exit (exitFailure)
import GHC.Generics (Generic)
import Game.Types (Tile(..), charToTile) 

-- | Tipo de dado que define a estrutura de um único nível no JSON.
-- | tileMap: O mapa do nível;
-- | marks: As posições das marcas;
-- | deriving (Show, Generic): 'Show' para exibição e 'Generic' para que Aeson possa decodificar automaticamente.
data LevelLocal = LevelLocal
  { tileMap :: [String],
    marks :: [[Int]]
  } deriving (Show, Generic)

-- | Tipo de dado que define o wrapper para o arquivo JSON completo.
-- | Contém um campo 'levelsLocal' que é uma lista de 'LevelLocal'.
-- | deriving (Show, Generic): 'Show' para exibição e 'Generic' para que Aeson possa decodificar automaticamente.
data MapWrapperLocal = MapWrapperLocal
  { levels :: [LevelLocal]
  } deriving (Show, Generic)


-- | Declara que o tipo 'LevelLocal' pode ser decodificado a partir de JSON.
instance Ae.FromJSON LevelLocal 

-- | Declara que o tipo 'MapWrapperLocal' pode ser decodificado a partir de JSON.
instance Ae.FromJSON MapWrapperLocal


-- | Função que carrega o mapa a partir de um arquivo JSON.
-- | @param path FilePath: O caminho do arquivo JSON.
-- | @param index Int: O índice do nível a ser carregado 
-- | @return IO (A.Array (Int, Int) Tile, (Int, Int), [(Int, Int)]): Retorna uma tupla com o mapa, a posição do jogador e as posições das marcas.
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
                    rows = tileMap levelData 
                    markPositions = map (\[y, x] -> (y, x)) (marks levelData) 
                
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
                
                return (A.array ((0, 0), (numRows - 1, numCols - 1)) assocs, playerPos, markPositions)
        
        Nothing -> do
            putStrLn "Falha ao decodificar o JSON! Verifique o formato."
            putStrLn $ "Conteúdo bruto lido:\n" ++ show content
            exitFailure