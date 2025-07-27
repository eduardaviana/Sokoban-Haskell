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

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- === Loop do jogo ===
gameLoop :: A.Array (Int, Int) Tile -> (Int, Int) -> IO ()
gameLoop gameMap currentPlayerPos = do
    clearScreen
    putStrLn "=== SOKOBAN ==="
    printMap gameMap currentPlayerPos
    putStrLn $ "Posição: " ++ show currentPlayerPos
    putStrLn "Use w/a/s/d para mover, q para sair"

    tecla <- getChar
    _ <- getChar -- Consumir o '\n'

    if tecla == 'q'
        then putStrLn "Fim do jogo!"
        else
            let newPos = move tecla currentPlayerPos gameMap
            in gameLoop gameMap newPos


-- === Início do jogo ===
start :: String -> IO ()
start dificuldadeAtual = do
    cwd <- getCurrentDirectory
    let jsonPath = cwd </> "data/maps/" ++ dificuldadeAtual
    gameMap <- loadMapFromJSON jsonPath
    gameLoop gameMap (4, 4)
