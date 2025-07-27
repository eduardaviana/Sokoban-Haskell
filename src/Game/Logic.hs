module Logic ( 
    direcao,
    move,
    printMap
) where

import qualified Data.Array as A
import Types  
import SokobanMap 

direcao :: Char -> (Int, Int)
direcao 'w' = (-1, 0)
direcao 'a' = (0, -1)
direcao 's' = (1, 0)
direcao 'd' = (0, 1)
direcao _   = (0, 0)

move :: Char -> (Int, Int) -> A.Array (Int, Int) Tile -> (Int, Int)
move tecla (y, x) gameMap =
    let (dy, dx) = direcao tecla
        newPos   = (y + dy, x + dx)
    in if inBounds (A.bounds gameMap) newPos && gameMap A.! newPos /= Wall
       then newPos
       else (y, x)

printMap :: A.Array (Int, Int) Tile -> (Int, Int) -> IO ()
printMap gameMap playerPos = do
    let ((minY, minX), (maxY, maxX)) = A.bounds gameMap
    mapM_ (\y -> do
        mapM_ (\x -> do
            let tile = if (y, x) == playerPos then Player else gameMap A.! (y, x)
            putChar (tileToChar tile)
            ) [minX..maxX]
        putStrLn ""
        ) [minY..maxY]
    putStrLn ""