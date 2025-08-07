-- | Módulo que contém a lógica central do jogo, inclui funções para a movimentação do jogador, verificação de vitória e progressão de dificuldade.
module Game.Logic where

import qualified Data.Array as A
import Game.Types
import Game.SokobanMap ()

-- | Função com casamento de padrão para os movimentos do jogador no mapa. Possui os parametros:
-- | @param tecla Char: A tecla de direção pressionada
-- | @return (Int, Int): Uma tupla com as mudanças de coordenadas
direcao :: Char -> (Int, Int)
direcao 'w' = (-1, 0)
direcao 'a' = (0, -1)
direcao 's' = (1, 0)
direcao 'd' = (0, 1)
direcao _   = (0, 0)

-- | Função com casamento de padrão para determinar a proxima dificuldade que o jogador deve ir
-- | @param dificuldadeAtual String: O nome do arquivo da dificuldade atual
-- | @return String: O nome do arquivo da sua próxima dificuldade, se não reconhecer a dificuldade por padrao irá para a facil
proximaDificuldade :: String -> String
proximaDificuldade "facil.json" = "medio.json"
proximaDificuldade "medio.json" = "dificil.json"
proximaDificuldade "dificil.json" = "dificil.json"
proximaDificuldade _ = "facil.json"

-- | Função para verificar se o jogador venceu o nivel, recebe como paramentros:
-- | @param gameMap A.Array (Int, Int) Tile: O mapa do jogo
-- | @param markPos [(Int, Int)]: As coordenadas das Marks onde as caixas devem ser movidas
-- | @return Bool: Retorna true se todas as caixas estiverem nas marcas corretas e false caso contrário
checaVitoria :: A.Array (Int, Int) Tile -> [(Int, Int)] -> Bool
checaVitoria _ [] = True
checaVitoria gameMap (x:xs)
    | not markN = False
    | otherwise = checaVitoria gameMap xs
    where markN = gameMap A.! x == Box

-- | Função que lida com o movimento do jogador e de empurrar a caixa, possui como parametros:
-- | @param isPlayer Bool: Indica se o que está tentando mover é o jogador (True) ou uma caixa sendo empurrada (False).
-- | @param tecla Char: A tecla de direção pressionada.
-- | @param pos (Int, Int): A posição atual do que está tentando mover (jogador ou caixa).
-- | @param gameMap A.Array (Int, Int) Tile: O mapa do jogo atual.
-- | @return ((Int, Int), (Int, Int)): Retorna uma tupla de tuplas com a nova posição da caixa e do jogador
move :: Bool -> Char -> (Int, Int) -> A.Array (Int, Int) Tile -> ((Int, Int), (Int, Int))
move isPlayer tecla (y, x) gameMap =
    let (dy, dx) = direcao tecla
        newPos   = (y + dy, x + dx)
    in if inBounds (A.bounds gameMap) newPos && gameMap A.! newPos /= Wall
       then 
            if gameMap A.! newPos == Box && isPlayer
                then let (_, boxNewPos) = move False tecla newPos gameMap
                     in if boxNewPos /= newPos && gameMap A.! boxNewPos /= Box
                        then (boxNewPos, newPos)  
                        else ((-1, -1), (y, x))    
            else ((-1, -1), newPos)
       else ((-1, -1), (y, x))

-- | Função que verifica os limites de um array, recebendo como parâmetros:
-- | @param bounds ((Int, Int), (Int, Int)): Uma tupla de tuplas que define os limites do array;
-- | @param pos (Int, Int): A coordenada (y, x) a ser verificada.
-- | @return Bool: Retorna True se a coordenada estiver dentro dos limites do array, e False caso contrário.
inBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inBounds ((minY, minX), (maxY, maxX)) (y, x) =
    x >= minX && x <= maxX &&
    y >= minY && y <= maxY



