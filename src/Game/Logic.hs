module Game.Logic (
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

checaVitoria :: A.Array (Int, Int) Tile -> [(Int, Int)] -> Bool -> Bool
checaVitoria gameMap [] filledMark = filledMark
checaVitoria gameMap (x:xs) filledMark 
    | not filledMark = False
    | not markN = False
    | otherwise = checaVitoria gameMap xs True
    where markN = gameMap A.! x == Box
    


move :: Bool -> Char -> (Int, Int) -> A.Array (Int, Int) Tile -> ((Int, Int), (Int, Int))
move isPlayer tecla (y, x) gameMap =
    let (dy, dx) = direcao tecla
        newPos   = (y + dy, x + dx)
    in if inBounds (A.bounds gameMap) newPos && gameMap A.! newPos /= Wall
       then if gameMap A.! newPos == Box && isPlayer
            then let (_, boxNewPos) = move False tecla newPos gameMap  -- move a caixa recursivamente, primeira posição é irrelevante aí pq a primeira representa uma caixa que teria sido afetada pelo movimento,como caixa não empurra caixa então o valor é sempre (-1,-1)
                 in if boxNewPos /= newPos && gameMap A.! boxNewPos /= Box -- se caixa se moveu
                    then (boxNewPos, newPos)  -- caixa -> boxNewPos e jogador -> newPos
                    else ((-1, -1), (y, x))  -- tinha alguma coisa na frete da caixa
            else ((-1, -1), newPos)  -- só jogador se mexe
       else ((-1, -1), (y, x))  -- movimento invalido

inBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inBounds ((minY, minX), (maxY, maxX)) (y, x) =
    x >= minX && x <= maxX && y >= minY && y <= maxY



