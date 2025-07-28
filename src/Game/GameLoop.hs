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
gameLoop :: A.Array (Int, Int) Tile -> (Int, Int) -> (Int, Int) ->IO ()
gameLoop gameMap currentPlayerPos pushedBoxPos = do
    --atualiza posição da caixa(se tiver sido empurrada) OQ VCS ACHAM DE TESTAR ISSO COM O PLAYER TBM?
    let updatedMap = if pushedBoxPos /= (-1, -1) 
                     then gameMap A.// [(currentPlayerPos, Floor), (pushedBoxPos, Box)]
                     else gameMap
    clearScreen
    putStrLn "=== SOKOBAN ==="
    printMap updatedMap currentPlayerPos
    putStrLn $ "Posição: " ++ show currentPlayerPos
    putStrLn "Use w/a/s/d para mover, q para sair"

    tecla <- getCharInstant

    if tecla == 'q'
        then putStrLn "Fim do jogo!"
        else
            let (boxNewPos, playerNewPos) = move True tecla currentPlayerPos updatedMap
            in gameLoop updatedMap playerNewPos boxNewPos


-- === Início do jogo ===
start :: String -> IO ()
start dificuldadeAtual = do
    cwd <- getCurrentDirectory
    let jsonPath = cwd </> "data/maps/" ++ dificuldadeAtual
    gameMap <- loadMapFromJSON jsonPath
    gameLoop gameMap (4, 4) (-1, -1)  -- (-1,-1) necessário por causa da auteração no gameLoop
