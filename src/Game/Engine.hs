-- | Módulo que contém a lógica central do jogo, inclui funções para a movimentação do jogador, verificação de vitória e progressão de dificuldade.
module Game.Engine (
    update, 
    isVictory,
    nextDifficulty
) where

import qualified Data.Array as A
import Game.Types


-- | Função com casamento de padrão para os movimentos do jogador no mapa. Possui os parametros:
-- | @param Direction: A direção escolhida pelo jogador
-- | @return (Int, Int): Uma tupla com as mudanças de coordenadas
directionToVector :: Direction -> (Int, Int)
directionToVector Up = (-1, 0)
directionToVector Down = (1, 0)
directionToVector GoLeft = (0, -1)
directionToVector GoRight = (0, 1)


-- | Função que calcula o índice do próximo nível.
-- | @param Int: O índice do nível atual.
-- | @return Int: O índice do próximo nível.
nextLevel :: Int -> Int
nextLevel level = if level < 4 then level + 1 else 0


-- | Função com casamento de padrão para determinar a proxima dificuldade que o jogador deve ir
-- | @param String: O nome do arquivo da dificuldade atual
-- | @return String: O nome do arquivo da sua próxima dificuldade, se não reconhecer a dificuldade por padrao irá para a facil
nextDifficulty :: String -> String
nextDifficulty "facil.json" = "medio.json"
nextDifficulty "medio.json" = "dificil.json"
nextDifficulty "dificil.json" = "dificil.json"
nextDifficulty _ = "facil.json"


-- | Ponto de entrada principal da lógica. Atualiza o estado do jogo baseado em uma ação.
-- | @param action Action: A ação a ser executada (mover, reiniciar, etc.).
-- | @param config GameConfig: A configuração imutável do nível.
-- | @param state GameState: O estado atual do jogo.
-- | @return GameState: O novo estado do jogo após a ação.
update :: Action -> GameConfig -> GameState -> GameState
update (Move direction) config state = handleMove direction config state
update _ _ state = state


-- | Verifica se a condição de vitória foi atingida.
-- | @param config GameConfig: A configuração do jogo.
-- | @param state GameState: O estado atual do jogo.
-- | @return Bool: True se o jogador venceu, False caso contrário.
isVictory :: GameConfig -> GameState -> Bool
isVictory config state =
    checkMarks (gcMarkPos config) (gsMap state)


-- | Função auxiliar que percorre a lista de posições dos alvos, verificando se cada um tem uma caixa.
-- | @param marks [(Int, Int)]: A lista de alvos a verificar.
-- | @param currentMap A.Array (Int, Int) Tile: O mapa do jogo.
-- | @return Bool: O resultado da verificação.
checkMarks :: [(Int, Int)] -> A.Array (Int, Int) Tile -> Bool
checkMarks [] _ = True
checkMarks (m:marks) currentMap
    | not (currentMap A.! m == Box) = False
    | otherwise = checkMarks marks currentMap


-- | Função que lida com a lógica de um movimento do jogador.
-- | @param direction Direction: A direção na qual o jogador quer se mover.
-- | @param config GameConfig: A configuração do nível.
-- | @param state GameState: O estado atual do jogo.
-- | @return GameState: O novo estado do jogo. Se o movimento for inválido, retorna o estado original.
handleMove :: Direction -> GameConfig -> GameState -> GameState
handleMove direction config state =
    let
        (dy, dx) = directionToVector direction
        (y, x)   = gsPlayerPos state
        newPlayerPos = (y + dy, x + dx)

        tileAtNewPos = (gsMap state) A.! newPlayerPos

    in
        case tileAtNewPos of
            Wall ->
                state
            Box ->
                let
                    posAfterBox = (fst newPlayerPos + dy, snd newPlayerPos + dx)
                    tileAfterBox = (gsMap state) A.! posAfterBox
                in
                    if tileAfterBox == Floor || tileAfterBox == Mark then
                        let newMap = updateMapAfterMove config state newPlayerPos (Just posAfterBox)
                        in GameState newMap newPlayerPos (gsMoveCount state + 1) (gsHistory state)
                    else
                        state
            _ ->
                let newMap = updateMapAfterMove config state newPlayerPos Nothing
                in GameState newMap newPlayerPos (gsMoveCount state + 1) (gsHistory state)


-- | Função auxiliar para criar o novo Array do mapa após um movimento.
-- | @param config GameConfig: A configuração do nível.
-- | @param oldState GameState: O estado antes do movimento.
-- | @param newPlayerPos (Int, Int): A nova posição do jogador.
-- | @param newBoxPos (Maybe (Int, Int)): A nova posição da caixa, se uma foi movida.
-- | @return A.Array (Int, Int) Tile: O novo grid do mapa.
updateMapAfterMove :: GameConfig -> GameState -> (Int, Int) -> Maybe (Int, Int) -> A.Array (Int, Int) Tile
updateMapAfterMove config oldState newPlayerPos maybeNewBoxPos =
    let
        oldPlayerPos = gsPlayerPos oldState
        originalMap = gsMap oldState
        tileAtOldPos = if oldPlayerPos `elem` gcMarkPos config then Mark else Floor
        playerUpdates = [ (oldPlayerPos, tileAtOldPos), (newPlayerPos, Player) ]
        finalUpdates = case maybeNewBoxPos of
            Just boxPos -> (boxPos, Box) : playerUpdates
            Nothing -> playerUpdates
    in
        originalMap A.// finalUpdates


-- | Função que verifica os limites de um array, recebendo como parâmetros:
-- | @param bounds ((Int, Int), (Int, Int)): Uma tupla de tuplas que define os limites do array;
-- | @param pos (Int, Int): A coordenada (y, x) a ser verificada.
-- | @return Bool: Retorna True se a coordenada estiver dentro dos limites do array, e False caso contrário.
inBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inBounds ((minY, minX), (maxY, maxX)) (y, x) =
    x >= minX && x <= maxX &&
    y >= minY && y <= maxY
