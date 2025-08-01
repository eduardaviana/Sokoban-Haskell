{-# LANGUAGE DeriveGeneric #-}

module Game.GameLoop where


import qualified Data.Array as A
import qualified Data.ByteString.Lazy as B
import Data.Aeson (decode)
import System.Exit (exitFailure)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

import Game.Types
import Game.Logic
import Game.SokobanMap
import Game.IO

-- === Loop do jogo ===
gameLoop :: A.Array (Int, Int) Tile -> [(Int, Int)] -> (Int, Int) -> (Int, Int) -> String -> Int ->IO ()
gameLoop gameMap markPos currentPlayerPos pushedBoxPos dificuldadeAtual level = do
    
    --atualiza posição da caixa(se tiver sido empurrada) OQ VCS ACHAM DE TESTAR ISSO COM O PLAYER TBM?
    let updatedMap = if pushedBoxPos /= (-1, -1) 
                     then gameMap A.// [(currentPlayerPos, Floor), (pushedBoxPos, Box)]
                     else gameMap
    -- if checaVitoria updatedMap markPos True
    --     then proximoNivel
    clearScreen
    putStrLn "=== SOKOBAN ==="
    printMap updatedMap currentPlayerPos
    putStrLn $ "Posição: " ++ show currentPlayerPos
    putStrLn "Use w/a/s/d para mover, q para sair"

    tecla <- getCharInstant

    if tecla == 'q'
        then putStrLn "Fim do jogo!"
        else if tecla == 'n'
            then do
                putStrLn "Trocando nível"
                newGameMap <- loadMapFromJSON ("data/maps/" ++ dificuldadeAtual) (proximoNivel level)
                gameLoop newGameMap markPos (4, 4) (-1, -1) dificuldadeAtual (proximoNivel level)

            else if tecla == 'r'
                then do
                    putStrLn "Reiniciando nível"
                    newGameMap <- loadMapFromJSON ("data/maps/" ++ dificuldadeAtual) level
                    gameLoop newGameMap markPos (4, 4) (-1, -1) dificuldadeAtual level

            else 
                let (boxNewPos, playerNewPos) = move True tecla currentPlayerPos updatedMap
                in gameLoop updatedMap markPos playerNewPos boxNewPos dificuldadeAtual level

-- === Início do jogo ===
start :: String -> Int -> IO ()
start dificuldadeAtual level = do
    cwd <- getCurrentDirectory
    let jsonPath = cwd </> "data/maps/" ++ dificuldadeAtual
    gameMap <- loadMapFromJSON jsonPath level
    --fazer função getMarks pra pegar a posição das marks no json
    -- depois substituir o array na chamada do gameloop abaixo pelo getmark
    --gameLoop gameMap (getMark gamemap) (4, 4) (-1, -1) dificuldadeAtual level
    gameLoop gameMap [] (4, 4) (-1, -1) dificuldadeAtual level  -- (-1,-1) necessário por causa da auteração no gameLoop
