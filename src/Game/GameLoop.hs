-- | Este módulo contém a lógica do loop principal do jogo Sokoban,
-- | responsável por gerenciar a interação do jogador, o estado do jogo e a
-- | progressão de níveis.
-- |
-- | O fluxo principal é orquestrado pela função 'gameLoop', que se chama
-- | recursivamente até que uma condição de término (vitória ou desistência)
-- | seja alcançada.
module Game.GameLoop (start, getCharInstant) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import System.IO
import System.FilePath (takeBaseName)
import Control.Concurrent (threadDelay)
import Game.Types
import Game.Engine
import Game.View
import Game.MapLoader
import Game.Utils


-- | Ponto de entrada para iniciar uma partida a partir do menu principal.
-- | Carrega o mapa inicial e então invoca o 'gameLoop' principal.
-- | @param difficulty String: O nome do arquivo da dificuldade (ex: "facil.json").
-- | @param level Int: O índice do nível a ser carregado (começando em 0).
-- | @return IO (): Uma ação de IO que roda o jogo completo.
start :: String -> Int -> IO ()
start difficulty level = do
    putStrLn "Carregando nível..."
    -- Usa o MapLoader para obter a configuração e o estado inicial.
    (config, state) <- loadLevel difficulty level
    -- Inicia o loop principal do jogo com os dados carregados.
    gameLoop config state


-- | O loop principal e recursivo do jogo.
-- | A cada iteração, ele verifica a vitória, renderiza a tela e processa o input do jogador para determinar
-- | o próximo estado.
-- | @param config GameConfig: A configuração imutável do nível.
-- | @param state GameState: O estado atual do jogo.
-- | @return IO (): Uma ação de I/O que continua o jogo.
gameLoop :: GameConfig -> GameState -> IO ()
gameLoop config state = do
    render config state
    if isVictory config state
        then handleVictory config
        else do
            input <- getCharInstant
            let action = parseAction input
            handleAction action config state


-- | Processa a ação do jogador, decidindo o que fazer a seguir.
-- | @param action Action: A ação interpretada a partir do input do jogador.
-- | @param config GameConfig: A configuração do nível.
-- | @param state GameState: O estado atual do jogo.
-- | @return IO (): Uma ação de I/O que avança o jogo.
handleAction :: Action -> GameConfig -> GameState -> IO ()
handleAction action config state =
    case action of
        Quit -> putStrLn "Saindo do nível..."
        Restart -> do
            putStrLn "Reiniciando nível..."
            threadDelay 500000
            start (gcDifficulty config) (gcLevel config)
        NoOp ->
            gameLoop config state
        Undo ->
            case gsHistory state of
                (lastState:_) -> gameLoop config lastState
                [] -> gameLoop config state
        moveAction@(Move _) ->
            let newState = update moveAction config state
            in gameLoop config newState { gsHistory = state : gsHistory newState }


-- | Lida com a lógica de transição após o jogador vencer um nível.
-- | Avança para o próximo nível, para a próxima dificuldade ou encerra o jogo.
-- | @param config GameConfig: A configuração do nível que foi concluído.
-- | @return IO (): Uma ação de I/O que inicia o próximo estágio do jogo.
handleVictory :: GameConfig -> IO ()
handleVictory config = do
    putStrLn "Sucesso! Nível completo!"
    threadDelay 1000000
    let currentDiff = gcDifficulty config
        nextLevelIndex = gcLevel config + 1

    if nextLevelIndex >= 5 then do
        putStrLn $ "Você completou todos os níveis da dificuldade '" ++ takeBaseName currentDiff ++ "'!"
        threadDelay 1000000
        if currentDiff == "dificil.json" then
            putStrLn "PARABÉNS! Você completou o jogo!"
        else do
            let newDiff = nextDifficulty currentDiff
            putStrLn $ "Avançando para a dificuldade '" ++ takeBaseName newDiff ++ "'..."
            threadDelay 2000000
            start newDiff 0
    else do
        putStrLn "Passando para o próximo nível..."
        threadDelay 1000000
        start currentDiff nextLevelIndex


-- | Mapeia caracteres de input para Ações do jogo.
actionMap :: Map.Map Char Action
actionMap = Map.fromList
    [ ('w', Move Up), ('W', Move Up), 
		  ('a', Move GoLeft), ('A', Move GoLeft),
		  ('s', Move Down), ('S', Move Down), 
			('d', Move GoRight), ('D', Move GoRight), 
			('q', Quit), ('Q', Quit), 
			('r', Restart), ('R', Restart), 
			('u', Undo), ('U', Undo)
    ]
    

-- | Converte um Char de input em uma Ação de jogo usando o mapa.
-- | @param char Char: O caractere pressionado pelo usuário.
-- | @return Action: A ação de jogo correspondente.
parseAction :: Char -> Action
parseAction char = fromMaybe NoOp (Map.lookup char actionMap)
