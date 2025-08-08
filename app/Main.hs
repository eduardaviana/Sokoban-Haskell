-- | Módulo principal do executável.
-- | Responsável exclusivamente por gerenciar o menu inicial e iniciar o jogo através do Game.GameLoop.
module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Game.GameLoop (start)
import Game.Utils (clearScreen, capitalize, drawBottomBorder, drawMiddleBorder, drawTopBorder, getCharInstant)
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
    putStr ">>> Escolha uma opção: "
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
    putStrLn "Iniciando o jogo..."
    threadDelay 700000
    start (currentDifficulty state) (currentLevel state)
    putStrLn "\nPressione qualquer tecla para voltar ao menu principal..."
    _ <- getCharInstant
    return state

changeDifficulty :: MenuAction
changeDifficulty state = do
    newDiff <- selectDifficulty
    putStrLn $ "Dificuldade definida como: " ++ takeBaseName newDiff
    threadDelay 1000000
    return $ state { currentDifficulty = newDiff }

changeLevel :: MenuAction
changeLevel state = do
    newLevel <- selectLevel
    putStrLn $ "Level selecionado: " ++ show (newLevel + 1)
    threadDelay 1000000
    return $ state { currentLevel = newLevel }

quitGame :: MenuAction
quitGame state = do
    putStrLn "Obrigado por jogar! :)"
    return $ state { currentLevel = -1 }

invalidOption :: MenuAction
invalidOption state = do
    putStrLn "Opção inválida! Tente novamente."
    threadDelay 1000000
    return state

displayMenu :: MenuState -> IO ()
displayMenu state = do
    clearScreen
    putStrLn "███████  ██████  ██   ██  ██████  ██████   █████  ███    ██"
    putStrLn "██      ██    ██ ██  ██  ██    ██ ██   ██ ██   ██ ████   ██"
    putStrLn "███████ ██    ██ █████   ██    ██ ██████  ███████ ██ ██  ██"
    putStrLn "     ██ ██    ██ ██  ██  ██    ██ ██   ██ ██   ██ ██  ██ ██"
    putStrLn "███████  ██████  ██   ██  ██████  ██████  ██   ██ ██   ████"
    putStrLn ""
    putStrLn "Bem-vindo ao Sokoban em Haskell!"
    putStrLn $ ">>> Configuração Atual: " ++ capitalize (takeBaseName (currentDifficulty state)) ++ ", Nível " ++ show (currentLevel state + 1)
    drawTopBorder 47
    putStrLn "║ MENU INICIAL                                ║"
    drawMiddleBorder 47
    putStrLn "║ 1. Iniciar Jogo                             ║"
    putStrLn "║ 2. Selecionar Dificuldade                   ║"
    putStrLn "║ 3. Selecionar Nível                         ║"
    putStrLn "║ 4. Sair                                     ║"
    drawBottomBorder 47

selectDifficulty :: IO String
selectDifficulty = do
    clearScreen
    putStrLn ">> Selecione a dificuldade:\n\n1. Fácil\n2. Médio\n3. Difícil"
    putStr ">> "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> return "facil.json"
        "2" -> return "medio.json"
        "3" -> return "dificil.json"
        _   -> putStrLn "Escolha inválida! Tente novamente." >> threadDelay 500000 >> selectDifficulty

selectLevel :: IO Int
selectLevel = do
    clearScreen
    putStrLn ">> Selecione o nível (1-5):"
    putStr ">> "
    hFlush stdout
    choice <- getLine
    case reads choice :: [(Int, String)] of
        [(n, "")] | n >= 1 && n <= 5 -> return (n - 1)
        _ -> putStrLn "Escolha inválida! Tente novamente." >> threadDelay 500000 >> selectLevel