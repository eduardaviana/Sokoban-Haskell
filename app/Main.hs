module Main where

import Game.GameLoop (start)
import Game.IO
import Control.Concurrent (threadDelay)

mostrarMenu :: IO()
mostrarMenu = do
    putStrLn "███████  ██████  ██   ██  ██████  ██████   █████  ███    ██"
    putStrLn "██      ██    ██ ██  ██  ██    ██ ██   ██ ██   ██ ████   ██"
    putStrLn "███████ ██    ██ █████   ██    ██ ██████  ███████ ██ ██  ██"
    putStrLn "     ██ ██    ██ ██  ██  ██    ██ ██   ██ ██   ██ ██  ██ ██"
    putStrLn "███████  ██████  ██   ██  ██████  ██████  ██   ██ ██   ████"
    putStrLn ""                                               
    putStrLn "Bem-vindo ao Sokoban em Haskell!"
    putStrLn "1. Entrar no Jogo"
    putStrLn "2. Selecionar Dificuldade"
    putStrLn "3. Sair"
    putStrLn "" 

dificuldade :: IO String
dificuldade = do
    clearScreen
    putStrLn "Selecione a dificuldade:"
    putStrLn "1. Fácil"
    putStrLn "2. Médio"
    putStrLn "3. Difícil"
    putStrLn ""

    escolha <- getLine

    case escolha of
        "1" -> return "easy.json"
        "2" -> return "medium.json"
        "3" -> return "hard.json"
        _   -> do
            putStrLn "Escolha inválida! Tente novamente."
            threadDelay 1000000
            dificuldade

quit :: IO()
quit = putStrLn "SAIU DO JOGO"

menu :: String -> String -> IO()
menu "options" dificuldadeAtual = do
    clearScreen
    mostrarMenu
    escolha <- getLine
    if tratarOpcao escolha "123"
        then menu escolha dificuldadeAtual
        else do
            putStrLn "Escolha inválida! Digite uma opção válida!"
            threadDelay 600000
            menu "options" dificuldadeAtual

menu "1" dificuldadeAtual = do 
    putStrLn "Iniciando o jogo..."
    threadDelay 500000
    start dificuldadeAtual
    threadDelay 500000
    menu "options" dificuldadeAtual

menu "2" _ = do
    novaDificuldade <- dificuldade
    putStrLn $ "Dificuldade definida como: " ++ novaDificuldade
    threadDelay 500000
    menu "options" novaDificuldade

menu "3" _ = quit

main :: IO()
main = menu "options" "easy.json"
