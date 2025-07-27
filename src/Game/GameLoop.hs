module GameLoop ( 
    gameLoop,
    start
) where

import qualified Data.Array as A
import Types    
import SokobanMap   
import Logic    
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Data.Char (digitToInt)
import System.IO (hFlush, stdout)

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

getSingleCharInput :: IO Char --funcao que corrige o erro de getChar mas temos que analisar
getSingleCharInput = do
    hFlush stdout
    line <- getLine
    if null line
        then return '\NUL'
        else return (head line)

gameLoop :: A.Array (Int, Int) Tile -> (Int, Int) -> IO ()
gameLoop gameMap currentPlayerPos = do
    clearScreen
    putStrLn "=== SOKOBAN ==="
    printMap gameMap currentPlayerPos
    putStrLn $ "Posição: " ++ show currentPlayerPos
    putStrLn "Use w/a/s/d para mover, q para sair"

    tecla <- getSingleCharInput

    --tecla <- getChar
    -- _ <- getChar -- Consumir o '\n' nao ta sendo suficiente 

    if tecla == 'q'
        then putStrLn "Fim do jogo!"
        else
            let newPos = move tecla currentPlayerPos gameMap
            in gameLoop gameMap newPos

start :: IO ()
start = do
    
    let getLevelChoice :: IO Int
        getLevelChoice = do
                --clearScreen
                putStrLn "=== SELEÇÃO DE FASE ==="
                putStrLn "1. Fase Fácil"
                putStrLn "2. Fase Média"
                putStrLn "3. Fase Difícil"
                putStrLn "4. Não quero jogar"
                putStrLn "Escolha uma fase (1-3):"

                --choiceStr <- getChar
                --_ <- getChar
                choiceStr <- getSingleCharInput

                let choiceInt = digitToInt choiceStr

                if choiceInt >= 1 && choiceInt <= 3
                    then return choiceInt 
                    else if choiceInt == 4
                        then return 0    
                    else do
                        putStrLn "Número fora do intervalo (1-3). Tente novamente."
                        getLevelChoice 

    chosenLevel <- getLevelChoice

    if chosenLevel == 0
        then putStrLn "" -- Mensagem de saída
        else do
            let mapFileName = case chosenLevel of
                                    1 -> "easy.json"
                                    2 -> "medium.json"
                                    3 -> "hard.json"
                                    _ -> "easy.json"

            cwd <- getCurrentDirectory
            let jsonPath = cwd </> "data" </> "maps" </> mapFileName
            gameMap <- loadMapFromJSON jsonPath
            gameLoop gameMap (4, 4)