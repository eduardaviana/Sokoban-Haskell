module Main where

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

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

tratarOpcao :: String -> Bool
tratarOpcao escolha
    | all (elem "123") escolha = True
    | otherwise = False

gameLoop :: IO()
gameLoop = putStrLn "ENTROU JOGO"

dificuldade :: IO()
dificuldade = putStrLn "ALTERAR DIFICULDADE"

quit :: IO()
quit = putStrLn "SAIU DO JOGO"

menu :: String -> IO()
menu "init" = do
    clearScreen
    mostrarMenu
    escolha <- getLine
    if tratarOpcao escolha
        then menu escolha
        else do
            putStrLn "Escolha inválida! Digite uma opção válida!"
            threadDelay 600000
            menu "init"

menu "1" = gameLoop
menu "2" = dificuldade
menu "3" = quit

main :: IO()
main = menu "init"
