--{-# LANGUAGE DeriveGeneric #-}

-- | Este módulo contém a lógica do loop principal do jogo responsavel por gerenciar a interação do jogador, o estado do jogo e a progressão de níveis. 
module Game.GameLoop where

import qualified Data.Array as A
--import qualified Data.ByteString.Lazy as B
--import Data.Aeson (decode)
--import System.Exit (exitFailure)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), takeBaseName)
import Control.Concurrent (threadDelay)

import Game.Types
import Game.Logic
import Game.SokobanMap
import Game.IO

-- Função de loop principal do jogo, que recebe como parametros: 
-- | @param gameMap A.Array (Int, Int) Tile: O mapa atual do jogo.
-- | @param markPos [(Int, Int)]: A lista de posições das marcas (alvos).
-- | @param currentPlayerPos (Int, Int): A posição atual do jogador.
-- | @param dificuldadeAtual String: A string que representa a dificuldade atual (e.g., "easy.json").
-- | @param level Int: O índice do nível atual dentro da dificuldade (0-indexado).
gameLoop :: A.Array (Int, Int) Tile -> [(Int, Int)] -> (Int, Int) -> String -> Int ->IO ()
gameLoop gameMap markPos currentPlayerPos dificuldadeAtual level = do
    --Verifica se o jogador ganhou chamando a função checaVitoria do Logic
    if checaVitoria gameMap markPos
        -- Se o jogador tiver ganhado o nível ou ele irá para o próximo da dificuldade ou completou o jogo
        then do
            putStrLn "Sucesso!"
            let proximoNivel = level + 1
            if proximoNivel >= 5
                then do
                    putStrLn $ "Você completou todos os níveis da dificuldade " ++ (takeBaseName dificuldadeAtual) ++ "!"
                    if dificuldadeAtual == "hard.json"
                        then do
                            putStrLn "Parabéns por completar o jogo!"
                            threadDelay 1000000
                            -- exitSuccess
                    else do
                        putStrLn $ "Avançando para a dificuldade " ++ (takeBaseName (proximaDificuldade dificuldadeAtual))
                        threadDelay 2000000
                        (newGameMap, newPlayerPos, newMarks) <- loadMapFromJSON ("data/maps/" ++ (proximaDificuldade dificuldadeAtual)) 0
                        gameLoop newGameMap newMarks newPlayerPos (proximaDificuldade dificuldadeAtual) 0
            else do
                putStrLn "Passando para o próximo nível..."
                threadDelay 1000000
                let proximoNivel = level + 1
                (newGameMap, newPlayerPos, newMarks) <- loadMapFromJSON ("data/maps/" ++ dificuldadeAtual) proximoNivel
                gameLoop newGameMap newMarks newPlayerPos dificuldadeAtual proximoNivel
        -- Se o jogador ainda não tiver ganhado é exibido na tela o mapa com o jogador e suas coordenadas no jogo
        else do
            clearScreen
            putStrLn "=== SOKOBAN ==="
            putStrLn $ "Dificuldade: " ++ (takeBaseName dificuldadeAtual) ++ " | Nível: " ++ show (level + 1)
            
            printMap gameMap currentPlayerPos markPos
            putStrLn $ "Posição: " ++ show currentPlayerPos
            putStrLn "Use w/a/s/d para mover, q para sair, r para reiniciar"

            --ler a decisão do jogador
            tecla <- getCharInstant

            -- q: sair do jogo
            if tecla == 'q'
                then putStrLn "Fim do jogo!"
            -- r: reiniciar nivel 
            else if tecla == 'r'
                then do
                    putStrLn "Reiniciando nível"
                    threadDelay 500000
                    (newGameMap, newPlayerPos, newMarks) <- loadMapFromJSON ("data/maps/" ++ dificuldadeAtual) level
                    gameLoop newGameMap newMarks newPlayerPos dificuldadeAtual level
            -- _: Tentativa de movimento no jogo (jogador/Caixa)
            else
                -- chama a função move do Logic para calcular a nova posição
                let (boxNewPos, playerNewPos) = move True tecla currentPlayerPos gameMap
                -- Verifica se foi possivel modificar o jogador 
                in if playerNewPos /= currentPlayerPos
                    --Se o jogador conseguiu se mexer guarda sua posição antiga e os Tile da posição anterior e da nova
                   then
                       let oldPlayerPos = currentPlayerPos
                           oldPlayerTile = gameMap A.! oldPlayerPos
                           newPlayerTile = gameMap A.! playerNewPos

                        -- Verifica se a nova posição do jogador é uma caixa, se sim a caixa foi empurrada.
                           isBoxPushed = newPlayerTile == Box

                        -- Decide o tile que ficará na posição antiga do jogador, se a posição antes era uma Mark ela volta a ser marca se não o tile é Floor
                           tileAtOldPlayerPos = if oldPlayerPos `elem` markPos
                                                then Mark
                                                else Floor
                        -- Atualiza mapa após o movimento
                           updatedMap = 
                            -- Se a caixa foi empurrada atualiza o gameMap passando os novos parametros para os antigos Tiles do jogador e caixa e a nova posição da caixa
                               if isBoxPushed
                                   then 
                                       let oldBoxPos = playerNewPos
                                           tileAtOldBoxPos = if oldBoxPos `elem` markPos
                                                             then Mark
                                                             else Floor
                                           newMap = gameMap A.// [(oldPlayerPos, tileAtOldPlayerPos), (oldBoxPos, tileAtOldBoxPos), (boxNewPos, Box)]
                                       in newMap
                            -- Se nenhuma caixa foi empurrada atualiza o gameMap apenas para o antigo Tile do jogador e a nova posição do jogador
                                   else 
                                       gameMap A.// [(oldPlayerPos, tileAtOldPlayerPos), (playerNewPos, Player)]
                        in gameLoop updatedMap markPos playerNewPos dificuldadeAtual level
                    -- Se não foi possivel modificar o jogador chama novamente o gameLoop com os mesmo parametros
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
