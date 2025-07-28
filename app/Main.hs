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
    putStrLn "3. Selecionar Level"
    putStrLn "4. Sair"
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

level :: IO Int
level = do
    clearScreen
    putStrLn "Selecione o level de 1 a 5:"

    escolha <- getLine
    case escolha of
        "1" -> return 0
        "2" -> return 1
        "3" -> return 2
        "4" -> return 3
        "5" -> return 4 
        _ -> do
            putStrLn "Escolha inválida! Tente novamente."
            threadDelay 1000000
            level

quit :: IO()
quit = putStrLn "SAIU DO JOGO"

menu :: String -> String -> Int -> IO()
menu "options" dificuldadeAtual levelAtual = do
    clearScreen
    mostrarMenu
    escolha <- getLine
    if tratarOpcao escolha "1234"
        then menu escolha dificuldadeAtual levelAtual
        else do
            putStrLn "Escolha inválida! Digite uma opção válida!"
            threadDelay 600000
            menu "options" dificuldadeAtual levelAtual

menu "1" dificuldadeAtual levelAtual = do 
    putStrLn "Iniciando o jogo..."
    threadDelay 500000
    start dificuldadeAtual levelAtual
    threadDelay 500000
    menu "options" dificuldadeAtual levelAtual

menu "2" _ levelAtual = do
    novaDificuldade <- dificuldade
    putStrLn $ "Dificuldade definida como: " ++ novaDificuldade
    threadDelay 500000
    menu "options" novaDificuldade levelAtual

menu "3" dificuldadeAtual _ = do
    novoLevel <- level
    putStrLn $ "Level selecionado: " ++ show (novoLevel + 1)
    threadDelay 500000
    menu "options" dificuldadeAtual novoLevel

menu "4" _ _ = quit

main :: IO()
main = menu "options" "easy.json" 0
