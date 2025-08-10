-- | Módulo que contém funções utilitárias para interação com o terminal e formatação.
module Game.Utils (
    cleanTerminal,
    capitalize,
    drawBottomBorder,
    drawMiddleBorder,
    drawTopBorder,
    getCharInstant,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    bold 
) where

import System.IO (hSetBuffering, BufferMode(LineBuffering), BufferMode(NoBuffering), stdin, stdout, hFlush, hSetEcho)
import System.Console.ANSI
import Data.Char (toUpper)

-- | Representa uma cor de terminal como uma lista de SGR (Select Graphic Rendition).
type TerminalColor = [SGR]

-- | Limpa a tela do terminal e posiciona o cursor no canto superior esquerdo.
-- | @return IO (): Uma ação de I/O que limpa a tela.
cleanTerminal :: IO ()
cleanTerminal = do
    clearScreen
    setCursorPosition 0 0


-- | Função utilizada para capitalizar a primeira letra de uma String
-- | @param palavra String: Uma String qualquer
-- | @return String: A String capitalizada
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs


-- | Desenha a borda superior de uma caixa no terminal, aplicando uma função de formatação.
-- | @param colorFunc (String -> String): A função de formatação a ser aplicada na borda.
-- | @param width Int: A largura total da caixa, incluindo as bordas.
-- | @return IO (): Uma ação de IO que imprime a linha da borda formatada.
drawTopBorder :: (String -> String) -> Int -> IO ()
drawTopBorder colorFunc width =
    putStrLn $ colorFunc ("╔" ++ replicate (width - 2) '═' ++ "╗")


-- | Desenha uma borda divisória, aplicando uma função de formatação.
-- | @param colorFunc (String -> String): A função de formatação a ser aplicada na borda.
-- | @param width Int: A largura total da caixa, incluindo as bordas.
-- | @return IO (): Uma ação de IO que imprime a linha da borda formatada.
drawMiddleBorder :: (String -> String) -> Int -> IO ()
drawMiddleBorder colorFunc width =
    putStrLn $ colorFunc ("╠" ++ replicate (width - 2) '═' ++ "╣")


-- | Desenha a borda inferior de uma caixa, aplicando uma função de formatação.
-- | @param colorFunc (String -> String): A função de formatação a ser aplicada na borda.
-- | @param width Int: A largura total da caixa, incluindo as bordas.
-- | @return IO (): Uma ação de IO que imprime a linha da borda formatada.
drawBottomBorder :: (String -> String) -> Int -> IO ()
drawBottomBorder colorFunc width =
    putStrLn $ colorFunc ("╚" ++ replicate (width - 2) '═' ++ "╝")


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


-- | Função para cores 
-- | @param text String: O texto a ser formatado.
-- | @return String: A string formatada com os códigos de cor.
red, green, yellow, blue, magenta, cyan, white :: String -> String
red text = setSGRCode [SetColor Foreground Vivid Red]      ++ text ++ setSGRCode [Reset]
green text = setSGRCode [SetColor Foreground Vivid Green]    ++ text ++ setSGRCode [Reset]
yellow text = setSGRCode [SetColor Foreground Vivid Yellow]  ++ text ++ setSGRCode [Reset]
blue text = setSGRCode [SetColor Foreground Vivid Blue]    ++ text ++ setSGRCode [Reset]
magenta text = setSGRCode [SetColor Foreground Vivid Magenta] ++ text ++ setSGRCode [Reset]
cyan text = setSGRCode [SetColor Foreground Vivid Cyan]    ++ text ++ setSGRCode [Reset]
white text = setSGRCode [SetColor Foreground Vivid White]   ++ text ++ setSGRCode [Reset]

-- | Função para formatação
-- | @param text String: O texto a ser formatado.
-- | @return String: A string formatada em negrito.
bold :: String -> String
bold text = setSGRCode [SetConsoleIntensity BoldIntensity] ++ text ++ setSGRCode [Reset]
