-- | Habilita a extensão 'DeriveGeneric' para a derivação automática de instâncias de 'FromJSON'.
{-# LANGUAGE DeriveGeneric #-}

-- | Módulo que lida com o carregamento de mapas a partir de arquivos JSON.
module Game.MapLoader (loadLevel) where

import qualified Data.Array as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as Ae
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import Game.Types


-- | Converte as strings do mapa em um Array e encontra a posição do jogador.
-- | @param rows [String]: As linhas do mapa lidas do arquivo.
-- | @return ((Int, Int), A.Array (Int, Int) Tile): Uma tupla contendo a posição inicial do jogador e o mapa do jogo como um Array.
parseMap :: [String] -> ((Int, Int), A.Array (Int, Int) Tile)
parseMap rows =
    let
        (playerPos, mapList) = foldl
            (\(pPos, accMap) (y, row) ->
                let (posInRow, newRow) = foldl
                        (\(p, r) (x, char) ->
                            if char == '@'
                                then ((y, x), r ++ [charToTile ' '])
                                else (p, r ++ [charToTile char])
                        ) (pPos, []) (zip [0..] row)
                in (posInRow, accMap ++ [newRow])
            ) ((-1, -1), []) (zip [0..] rows)

        numRows = length mapList
        numCols = if null mapList then 0 else length (head mapList)
        bounds  = ((0, 0), (numRows - 1, numCols - 1))
        assocs  = [ ((y, x), mapList !! y !! x) | y <- [0..numRows - 1], x <- [0..numCols - 1] ]
    in
        (playerPos, A.array bounds assocs)


-- | Função que carrega o mapa a partir de um arquivo JSON.
-- | @param difficultyName String: O nome do arquivo da dificuldade (ex: "facil.json").
-- | @param levelIndex Int: O índice do nível a ser carregado (começando em 0).
-- | @return IO (GameConfig, GameState): Retorna em IO a configuração e o estado inicial do jogo.
loadLevel :: String -> Int -> IO (GameConfig, GameState)
loadLevel difficultyName levelIndex = do
    cwd <- getCurrentDirectory
    let path = cwd </> "data" </> "maps" </> difficultyName
    content <- B.readFile path
    case Ae.decode content of
        Just (MapWrapperLocal lvls) ->
            if levelIndex < 0 || levelIndex >= length lvls then do
                putStrLn $ "Índice de nível inválido: " ++ show levelIndex
                exitFailure
            else
                let
                    levelData     = lvls !! levelIndex
                    rows          = tileMap levelData
                    markPositions = map (\[y, x] -> (y, x)) (marks levelData)
                    (playerPos, mapGrid) = parseMap rows
                in
                    return
                        ( GameConfig difficultyName levelIndex markPositions
                        , GameState mapGrid playerPos 0 []
                        )
        Nothing -> do
            putStrLn $ "Falha ao decodificar o JSON: " ++ path
            exitFailure
