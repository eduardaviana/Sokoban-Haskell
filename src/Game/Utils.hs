module Game.Utils (
    clearScreen, 
    capitalize,
    drawBottomBorder,
    drawMiddleBorder,
    drawTopBorder,
    getCharInstant
) where

import System.IO (hSetBuffering, BufferMode(LineBuffering), BufferMode(NoBuffering), stdin, stdout, hFlush, hSetEcho)
import Data.Char (toUpper)


-- | Limpa a tela do terminal e posiciona o cursor no canto superior esquerdo.
-- | @return IO (): Uma ação de I/O que limpa a tela.
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"


-- | Função pura de formatação, utilizada para capitalizar a primeira letra de uma String
-- | @param palavra String: Uma String qualquer
-- | @return String: A String capitalizada
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs


-- | Desenha a borda superior de uma caixa no terminal. Ex: ╔═════╗
-- | @param width Int: A largura total da caixa, incluindo as bordas.
-- | @return IO (): Uma ação de IO que imprime a linha da borda.
drawTopBorder :: Int -> IO ()
drawTopBorder width = putStrLn $ "╔" ++ replicate (width - 2) '═' ++ "╗"


-- | Desenha uma borda divisória no meio de uma caixa. Ex: ╠═════╣
-- | @param width Int: A largura total da caixa, incluindo as bordas.
-- | @return IO (): Uma ação de IO que imprime a linha da borda.
drawMiddleBorder :: Int -> IO ()
drawMiddleBorder width = putStrLn $ "╠" ++ replicate (width - 2) '═' ++ "╣"


-- | Desenha a borda inferior de uma caixa no terminal. Ex: ╚═════╝
-- | @param width Int: A largura total da caixa, incluindo as bordas.
-- | @return IO (): Uma ação de IO que imprime a linha da borda.
drawBottomBorder :: Int -> IO ()
drawBottomBorder width = putStrLn $ "╚" ++ replicate (width - 2) '═' ++ "╝"


-- | Lê um único caractere do teclado de forma instantânea, sem esperar por Enter.
-- | @return IO Char: Uma ação de I/O que retorna o caractere lido.
getCharInstant :: IO Char
getCharInstant = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    c <- getChar
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
    return c