-- | Módulo que lida com as operações de entrada e saída (I/O) do jogo 
module Game.IO where 

import System.IO
import qualified Data.Array as A
import Game.Types 

-- | Função que limpa a tela do terminal.
-- | @return IO (): Uma ação de I/O que limpa a tela.
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- | Função que valida se uma escolha de input é válida dentro de uma sequência de opções.
-- | @param escolha String: A string digitada pelo usuário.
-- | @param seq String: Uma sequência de caracteres válidos 
-- | @return Bool: Retorna True se todos os caracteres da 'escolha' estiverem presentes na 'seq', e False caso contrário.
tratarOpcao :: String -> String -> Bool
tratarOpcao escolha seq = all (\c -> c `elem` seq) escolha

-- | Função que lê um único caractere do teclado de forma instantânea, sem esperar por Enter.
-- | @return IO Char: Uma ação de I/O que retorna o caractere lido.
getCharInstant :: IO Char
getCharInstant = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    c <- getChar
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
    return c

-- | Função que imprime o mapa do jogo no terminal. 
-- | @param gameMap A.Array (Int, Int) Tile:  O mapa do jogo atual.
-- | @param playerPos (Int, Int): A posição atual do jogador.
-- | @param marks [(Int, Int)]: Uma lista de coordenadas das posições das marcas (alvos das caixas).
-- | @return IO (): Uma ação de I/O que exibe o mapa.
printMap :: A.Array (Int, Int) Tile -> (Int, Int) -> [(Int, Int)] -> IO ()
printMap gameMap playerPos marks = do
    let ((minY, minX), (maxY, maxX)) = A.bounds gameMap
    mapM_ (\y -> do
        mapM_ (\x -> do
            let currentPos = (y, x)
            let tile = gameMap A.! currentPos
            
            if currentPos == playerPos
                then putChar 'P' 
            else if tile == Wall
                then putChar '█' 
            else if tile == Box
                then
                    if currentPos `elem` marks
                    then putChar '='
                    else putChar '#' 
            else if tile == Floor
                then
                    if currentPos `elem` marks
                    then putChar 'x' 
                    else putChar ' ' 
            else if tile == Wall
                then
                    if currentPos `elem` marks
                    then putChar 'x' 
                    else putChar ' ' 
            else
                putChar (tileToChar tile)
            ) [minX..maxX]
        putStrLn ""
        ) [minY..maxY]
    putStrLn ""

-- | Função que calcula o índice do próximo nível.
-- | @param nivel Int: O índice do nível atual.
-- | @return Int: O índice do próximo nível.
proximoNivel :: Int -> Int
proximoNivel nivel = if nivel < 4 then nivel + 1 else 0