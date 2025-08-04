module Game.IO where 

import System.IO
import qualified Data.Array as A
import Game.Types (Tile(..), tileToChar)

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


printMap :: A.Array (Int, Int) Tile -> (Int, Int) -> [(Int, Int)] -> IO ()
printMap gameMap playerPos marks = do
    let ((minY, minX), (maxY, maxX)) = A.bounds gameMap
    mapM_ (\y -> do
        mapM_ (\x -> do
            let currentPos = (y, x)
            let tile = gameMap A.! currentPos
            
            if currentPos == playerPos
                then 
                    if currentPos `elem` marks 
                    then putChar '@' 
                    else putChar 'o' 
            else if tile == Box
                then
                    if currentPos `elem` marks
                    then putChar '*' 
                    else putChar '≡' 
            else if tile == Floor
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

proximoNivel :: Int -> Int
proximoNivel nivel = if nivel < 4 then nivel + 1 else 0