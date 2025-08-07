-- | Este módulo contém a lógica do loop principal do jogo responsavel por gerenciar a interação do jogador, o estado do jogo e a progressão de níveis. 
module Game.GameLoop where

import qualified Data.Array as A
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), takeBaseName)
import Control.Concurrent (threadDelay)
import Game.Types
import Game.Logic
import Game.SokobanMap
import Game.IO

-- Função de loop principal do jogo, que recebe como parametros: 
-- | @param gameMap A.Array (Int, Int) Tile: O mapa atual do jogo.
-- | @param markPos [(Int, Int)]: A lista de posições das marcas.
-- | @param currentPlayerPos (Int, Int): A posição atual do jogador.
-- | @param dificuldadeAtual String: A string que representa a dificuldade atual.
-- | @param level Int: O índice do nível atual dentro da dificuldade.
gameLoop :: A.Array (Int, Int) Tile -> [(Int, Int)] -> (Int, Int) -> String -> Int ->IO ()
gameLoop gameMap markPos currentPlayerPos dificuldadeAtual level = do
    if checaVitoria gameMap markPos
        then do
            putStrLn "Sucesso!"
            let proxNivel = level + 1
            if proxNivel >= 5
                then do
                    putStrLn $ "Você completou todos os níveis da dificuldade " ++ (takeBaseName dificuldadeAtual) ++ "!"
                    if dificuldadeAtual == "dificil.json"
                        then do
                            putStrLn "Parabéns por completar o jogo!"
                            threadDelay 1000000
                    else do
                        putStrLn $ "Avançando para a dificuldade " ++ (takeBaseName (proximaDificuldade dificuldadeAtual))
                        threadDelay 2000000
                        (newGameMap, newPlayerPos, newMarks) <- loadMapFromJSON ("data/maps/" ++ (proximaDificuldade dificuldadeAtual)) 0
                        gameLoop newGameMap newMarks newPlayerPos (proximaDificuldade dificuldadeAtual) 0
            else do
                putStrLn "Passando para o próximo nível..."
                threadDelay 1000000
                (newGameMap, newPlayerPos, newMarks) <- loadMapFromJSON ("data/maps/" ++ dificuldadeAtual) proxNivel
                gameLoop newGameMap newMarks newPlayerPos dificuldadeAtual proxNivel
        else do
            clearScreen
            putStrLn "=== SOKOBAN ==="
            putStrLn $ "Dificuldade: " ++ (takeBaseName dificuldadeAtual) ++ " | Nível: " ++ show (level + 1)
            
            printMap gameMap currentPlayerPos markPos
            putStrLn $ "Posição: " ++ show currentPlayerPos
            putStrLn "Use w/a/s/d para mover, q para sair, r para reiniciar"

            tecla <- getCharInstant

            if tecla == 'q'
                then putStrLn "Fim do jogo!" 
            else if tecla == 'r'
                then do
                    putStrLn "Reiniciando nível"
                    threadDelay 500000
                    (newGameMap, newPlayerPos, newMarks) <- loadMapFromJSON ("data/maps/" ++ dificuldadeAtual) level
                    gameLoop newGameMap newMarks newPlayerPos dificuldadeAtual level
            else
                let (boxNewPos, playerNewPos) = move True tecla currentPlayerPos gameMap
                in if playerNewPos /= currentPlayerPos
                   then
                       let oldPlayerPos = currentPlayerPos
                           newPlayerTile = gameMap A.! playerNewPos

                           isBoxPushed = newPlayerTile == Box

                           tileAtOldPlayerPos = if oldPlayerPos `elem` markPos
                                                then Mark
                                                else Floor
                           updatedMap = 
                               if isBoxPushed
                                   then 
                                       let oldBoxPos = playerNewPos
                                           tileAtOldBoxPos = if oldBoxPos `elem` markPos
                                                             then Mark
                                                             else Floor
                                           newMap = gameMap A.// [(oldPlayerPos, tileAtOldPlayerPos), (oldBoxPos, tileAtOldBoxPos), (boxNewPos, Box)]
                                       in newMap
                                   else 
                                       gameMap A.// [(oldPlayerPos, tileAtOldPlayerPos), (playerNewPos, Player)]
                        in gameLoop updatedMap markPos playerNewPos dificuldadeAtual level
                   else gameLoop gameMap markPos currentPlayerPos dificuldadeAtual level


-- Função que inicia o jogo, recebe como parametros:
-- | @param dificuldadeAtual String: O nome do arquivo JSON da dificuldade a ser carregada
-- | @param level Int: O índice do nível a ser carregado
-- | @return IO () Uma ação de I/O que inicia o loop principal do jogo.
start :: String -> Int -> IO ()
start dificuldadeAtual level = do
    cwd <- getCurrentDirectory
    let jsonPath = cwd </> "data/maps/" ++ dificuldadeAtual
    (gameMap, playerPos, marks) <- loadMapFromJSON jsonPath level
    gameLoop gameMap marks playerPos dificuldadeAtual level
