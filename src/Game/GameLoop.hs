{-# LANGUAGE DeriveGeneric #-}

module Game.GameLoop where

import qualified Data.Array as A
import qualified Data.ByteString.Lazy as B
import Data.Aeson (decode)
import System.Exit (exitFailure)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), takeBaseName)
import Control.Concurrent (threadDelay)

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

    if checaVitoria updatedMap markPos
        then do
            putStrLn "Sucesso!"
            let proximoNivel = level + 1
            if proximoNivel >= 5
                then do
                    putStrLn $ "Você completou todos os níveis da dificuldade " ++ (takeBaseName dificuldadeAtual) ++ "!" --take 5 pra tirar o .json do final
                    if dificuldadeAtual == "hard.json"
                        then do
                            putStrLn "Parabéns por completar o jogo!"
                            threadDelay 1000000
                            --exitSuccess
                    else do
                        putStrLn $ "Avançando para a dificuldade " ++ (takeBaseName (proximaDificuldade dificuldadeAtual))
                        threadDelay 2000000
                        (newGameMap, newMarks) <- loadMapFromJSON ("data/maps/" ++ (proximaDificuldade dificuldadeAtual)) 0
                        gameLoop newGameMap newMarks (4, 4) (-1, -1) (proximaDificuldade dificuldadeAtual) 0
            else do
                putStrLn "Passando para o próximo nível..."
                threadDelay 1000000
                let proximoNivel = level + 1
                (newGameMap, newMarks) <- loadMapFromJSON ("data/maps/" ++ dificuldadeAtual) proximoNivel
                gameLoop newGameMap newMarks (4, 4) (-1, -1) dificuldadeAtual proximoNivel
        else do
            clearScreen
            putStrLn "=== SOKOBAN ===" 
            putStrLn $ "Dificuldade: " ++ (takeBaseName dificuldadeAtual) ++ " | Nível: " ++ show (level + 1)
            printMap updatedMap currentPlayerPos
            putStrLn $ "Posição: " ++ show currentPlayerPos
            putStrLn "Use w/a/s/d para mover, q para sair, r para reiniciar"

            tecla <- getCharInstant

            if tecla == 'q'
                then putStrLn "Fim do jogo!"
            else if tecla == 'r'
                then do
                    putStrLn "Reiniciando nível"
                    threadDelay 500000
                    (newGameMap, newMarks) <- loadMapFromJSON ("data/maps/" ++ dificuldadeAtual) level
                    gameLoop newGameMap newMarks (4, 4) (-1, -1) dificuldadeAtual level

                else 
                    let (boxNewPos, playerNewPos) = move True tecla currentPlayerPos updatedMap
                    in gameLoop updatedMap markPos playerNewPos boxNewPos dificuldadeAtual level

-- === Início do jogo ===
start :: String -> Int -> IO ()
start dificuldadeAtual level = do
    cwd <- getCurrentDirectory
    let jsonPath = cwd </> "data/maps/" ++ dificuldadeAtual
    (gameMap, marks) <- loadMapFromJSON jsonPath level
    gameLoop gameMap marks (4, 4) (-1, -1) dificuldadeAtual level  -- (-1,-1) necessário por causa da auteração no gameLoop
