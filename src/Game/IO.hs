module Game.IO where 

import System.IO

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

tratarOpcao :: String -> String -> Bool
tratarOpcao escolha seq = all (\c -> c `elem` seq) escolha

getCharInstant :: IO Char
getCharInstant = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    c <- getChar
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
    return c
