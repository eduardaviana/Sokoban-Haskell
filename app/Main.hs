-- | Módulo principal do projeto responsável por gerenciar a navegação do menu inicial e iniciar a partida do jogo.
module Main where

-- | importação de outros modulos para utilizar suas funções
import Game.GameLoop (start)
import Game.IO
import Control.Concurrent (threadDelay)
import System.FilePath (takeBaseName)

-- | Função para exibir o menu principal do jogo
mostrarMenu :: IO()
mostrarMenu = do
    putStrLn "███████  ██████  ██   ██  ██████  ██████   █████  ███    ██"
    putStrLn "██      ██    ██ ██  ██  ██    ██ ██   ██ ██   ██ ████   ██"
    putStrLn "███████ ██    ██ █████   ██    ██ ██████  ███████ ██ ██  ██"
    putStrLn "     ██ ██    ██ ██  ██  ██    ██ ██   ██ ██   ██ ██  ██ ██"
    putStrLn "███████  ██████  ██   ██  ██████  ██████  ██   ██ ██   ████"
    putStrLn ""                                               
    putStrLn "Bem-vindo ao Sokoban em Haskell!"
    putStrLn " ------------------------------"
    putStrLn "| 1.      Entrar no Jogo       |"
    putStrLn "| 2.  Selecionar Dificuldade   |"
    putStrLn "| 3.      Selecionar Level     |"
    putStrLn "| 4.          Sair             |"
    putStrLn " ------------------------------"
    putStrLn "" 

-- | Função de menu para escolha de dificuldade
-- | @return Uma String contendo o nome do arquivo JSON da dificuldade selecionada.
dificuldade :: IO String
dificuldade = do
    clearScreen
    putStrLn "Selecione a dificuldade:"
    putStrLn " ---------------------"
    putStrLn "| 1.  Fácil           |"
    putStrLn "| 2.  Médio           |"
    putStrLn "| 3.  Difícil         |"
    putStrLn " ---------------------"
    putStrLn ""

    escolha <- getLine
    case escolha of
        "1" -> return "facil.json"
        "2" -> return "medio.json"
        "3" -> return "dificil.json"
        _   -> do
            putStrLn "Escolha inválida! Tente novamente."
            threadDelay 1000000
            dificuldade

-- | Função de menu para escolha de dificuldade
-- | @return Um Int que representa o índice do level selecionado.
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

-- | Função para imprimir mensagem de saída do jogo
quit :: IO()
quit = putStrLn "OBRIGADO POR JOGAR! :)"

-- | Função do menu principal para manipular as escolhas do jogador
-- | @param estadoAtual String: A string que representa o estado atual do menu.
-- | @param dificuldadeAtual String: A string do nome do arquivo da dificuldade atual.
-- | @param levelAtual Int: O índice do level selecionado.
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

-- | Entra diretamente no jogo chamando a função start 
menu "1" dificuldadeAtual levelAtual = do 
    putStrLn "Iniciando o jogo..."
    threadDelay 500000
    start dificuldadeAtual levelAtual
    threadDelay 500000
    menu "options" dificuldadeAtual levelAtual

-- | Se o jogador quiser selecionar a dificuldade é chamada a função de dificuldade
menu "2" _ levelAtual = do
    novaDificuldade <- dificuldade
    putStrLn $ "Dificuldade definida como: " ++ takeBaseName novaDificuldade
    threadDelay 500000
    menu "options" novaDificuldade levelAtual

-- | Se o jogador quiser selecionar o level é chamada a função level 
menu "3" dificuldadeAtual _ = do
    novoLevel <- level
    putStrLn $ "Level selecionado: " ++ show (novoLevel + 1)
    threadDelay 500000
    menu "options" dificuldadeAtual novoLevel

-- | Se o jogador quiser sair é selecionada a função quit
menu "4" _ _ = quit

-- | O menu deve ficar aparecendo até que uma das opções acima seja escolhida
menu _ dificuldadeAtual levelAtual = menu "options" dificuldadeAtual levelAtual 

-- | Chamada para o menu principal onde o jogo inicia na dificuldade facil nivel 1
main :: IO()
main = menu "options" "facil.json" 0
