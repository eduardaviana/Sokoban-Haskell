-- | Módulo principal do executável.
-- | Responsável exclusivamente por gerenciar o menu inicial e iniciar o jogo através do Game.GameLoop.
module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Game.GameLoop (start)
import Game.Utils (cleanTerminal, capitalize, drawBottomBorder, drawMiddleBorder, drawTopBorder, getCharInstant, bold, white, red, green, yellow, cyan)
import System.FilePath (takeBaseName)
import Control.Concurrent (threadDelay)
import System.IO (hSetBuffering, BufferMode(LineBuffering), stdout, hFlush)

data MenuState = MenuState
    { currentDifficulty :: String,
      currentLevel :: Int
    }

type MenuAction = MenuState -> IO MenuState

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    let initialState = MenuState "facil.json" 0
    runMenu initialState

runMenu :: MenuState -> IO ()
runMenu state = do
    displayMenu state
    putStr $ bold (white ">>> Escolha uma opção: ")
    hFlush stdout
    choice <- getLine
    let action = fromMaybe invalidOption (Map.lookup choice menuActions)
    newState <- action state
    if currentLevel newState < 0
        then return ()
        else runMenu newState

menuActions :: Map.Map String MenuAction
menuActions = Map.fromList
    [ ("1", runGame)
    , ("2", changeDifficulty)
    , ("3", changeLevel)
    , ("4", quitGame)
    ]

runGame :: MenuAction
runGame state = do
    putStrLn $ green "Iniciando o jogo..."
    threadDelay 700000
    start (currentDifficulty state) (currentLevel state)
    putStrLn $ cyan "Pressione qualquer tecla para voltar ao menu principal..."
    _ <- getCharInstant
    return state

changeDifficulty :: MenuAction
changeDifficulty state = do
    newDiff <- selectDifficulty
    putStrLn $ bold (white $ "Dificuldade definida como: " ++ takeBaseName newDiff)
    threadDelay 1000000
    return $ state { currentDifficulty = newDiff }

changeLevel :: MenuAction
changeLevel state = do
    newLevel <- selectLevel
    putStrLn $ bold (white $ "Level selecionado: " ++ show (newLevel + 1))
    threadDelay 1000000
    return $ state { currentLevel = newLevel }

quitGame :: MenuAction
quitGame state = do
    putStrLn $ green "Obrigado por jogar! :)"
    return $ state { currentLevel = -1 }

invalidOption :: MenuAction
invalidOption state = do
    putStrLn $ bold (red "Opção inválida! Tente novamente.")
    threadDelay 1000000
    return state

displayMenu :: MenuState -> IO ()
displayMenu state = do
    cleanTerminal
    putStrLn $ yellow "███████  ██████  ██   ██  ██████  ██████   █████  ███    ██"
    putStrLn $ yellow "██      ██    ██ ██  ██  ██    ██ ██   ██ ██   ██ ████   ██"
    putStrLn $ yellow "███████ ██    ██ █████   ██    ██ ██████  ███████ ██ ██  ██"
    putStrLn $ yellow "     ██ ██    ██ ██  ██  ██    ██ ██   ██ ██   ██ ██  ██ ██"
    putStrLn $ yellow "███████  ██████  ██   ██  ██████  ██████  ██   ██ ██   ████"
    putStrLn ""
    putStrLn $ cyan "Bem-vindo ao Sokoban em Haskell!"
    let currentConfigMsg = ">>> Configuração Atual: " ++ capitalize (takeBaseName (currentDifficulty state)) ++ ", Nível " ++ show (currentLevel state + 1)
    putStrLn $ bold (cyan currentConfigMsg)
    drawTopBorder white 47
    putStrLn $ white "║ MENU INICIAL                                ║"
    drawMiddleBorder white 47
    putStrLn $ white "║ 1. Iniciar Jogo                             ║"
    putStrLn $ white "║ 2. Selecionar Dificuldade                   ║"
    putStrLn $ white "║ 3. Selecionar Nível                         ║"
    putStrLn $ white "║ 4. Sair                                     ║"
    drawBottomBorder white 47


selectDifficulty :: IO String
selectDifficulty = do
    cleanTerminal
    putStrLn $ white ">> Selecione a dificuldade:"
    putStrLn $ green "1. Fácil"
    putStrLn $ yellow "2. Médio"
    putStrLn $ red "3. Difícil"
    putStr $ white ">> "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> return "facil.json"
        "2" -> return "medio.json"
        "3" -> return "dificil.json"
        _   -> putStrLn (red "Escolha inválida! Tente novamente.") >> threadDelay 500000 >> selectDifficulty


selectLevel :: IO Int
selectLevel = do
    cleanTerminal
    putStrLn $ white ">> Selecione o nível (1-5):"
    putStr $ white ">> "
    hFlush stdout
    choice <- getLine
    case reads choice :: [(Int, String)] of
        [(n, "")] | n >= 1 && n <= 5 -> return (n - 1)
        _ -> putStrLn (red "Escolha inválida! Tente novamente.") >> threadDelay 500000 >> selectLevel
