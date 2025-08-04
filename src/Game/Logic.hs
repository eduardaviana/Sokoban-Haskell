module Game.Logic (
    proximaDificuldade,
    direcao,
    move,
    checaVitoria
) where

import qualified Data.Array as A
import Game.Types
import Game.SokobanMap

direcao :: Char -> (Int, Int)
direcao 'w' = (-1, 0)
direcao 'a' = (0, -1)
direcao 's' = (1, 0)
direcao 'd' = (0, 1)
direcao _   = (0, 0)

proximaDificuldade :: String -> String
proximaDificuldade "easy.json" = "medium.json"
proximaDificuldade "medium.json" = "hard.json"
proximaDificuldade "hard.json" = "hard.json"
proximaDificuldade _ = "easy.json"

checaVitoria :: A.Array (Int, Int) Tile -> [(Int, Int)] -> Bool
checaVitoria gameMap [] = True
checaVitoria gameMap (x:xs)
    | not markN = False
    | otherwise = checaVitoria gameMap xs
    where markN = gameMap A.! x == Box
    
move :: Bool -> Char -> (Int, Int) -> A.Array (Int, Int) Tile -> ((Int, Int), (Int, Int))
move isPlayer tecla (y, x) gameMap =
    let (dy, dx) = direcao tecla
        newPos   = (y + dy, x + dx)
    in if inBounds (A.bounds gameMap) newPos && gameMap A.! newPos /= Wall
       then if gameMap A.! newPos == Box && isPlayer
            then let (_, boxNewPos) = move False tecla newPos gameMap 
                 in if boxNewPos /= newPos && gameMap A.! boxNewPos /= Box 
                    then (boxNewPos, newPos)  -- caixa -> boxNewPos e jogador -> newPos
                    else ((-1, -1), (y, x))  
            else ((-1, -1), newPos)  
       else ((-1, -1), (y, x)) 

inBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inBounds ((minY, minX), (maxY, maxX)) (y, x) =
    x >= minX && x <= maxX && y >= minY && y <= maxY



