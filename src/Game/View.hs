-- | Módulo responsável por toda a renderização do jogo no console.
module Game.View (render) where

import qualified Data.Array as A
import System.FilePath (takeBaseName)
import Game.Types
import Game.Utils (drawBottomBorder, drawMiddleBorder, drawTopBorder, cleanTerminal, capitalize, white)


-- | Função principal de renderização. Orquestra a limpeza e o desenho da tela completa do jogo.
-- | @param config GameConfig: A configuração imutável do nível (para exibir dificuldade, nível, etc.).
-- | @param state GameState: O estado atual e mutável do jogo (mapa, posição do jogador).
-- | @return IO (): Uma ação de I/O que desenha o jogo no terminal.
render :: GameConfig -> GameState -> IO ()
render config state = do 
    cleanTerminal

    let title = "SOKOBAN"
        level = "Nível: " ++ show (gcLevel config + 1)
        difficulty = "Dificuldade: " ++ capitalize (takeBaseName (gcDifficulty config))
        statusLine1 = level ++ " | " ++ difficulty

        totalBoxes = length (gcMarkPos config)
        boxesInPlace = length $ filter (\p -> (gsMap state) A.! p == Box) (gcMarkPos config)
        statusBoxes = "Caixas no Alvo: " ++ show boxesInPlace ++ "/" ++ show totalBoxes
        statusMoves = "Movimentos: " ++ show (gsMoveCount state)
        statusLine2 = statusBoxes ++ " | " ++ statusMoves

        help = "[W/A/S/D] Mover | [U] Desfazer | [R] Reiniciar | [Q] Sair"

    let ((_, minX), (_, maxX)) = A.bounds (gsMap state)
        mapWidth = maxX - minX + 1
        contentWidth = maximum [mapWidth, length statusLine1, length statusLine2, length help, length title]
        panelWidth = contentWidth + 4

    drawTopBorder white panelWidth
    putStrLn $ white ("║" ++ padCenter (panelWidth - 2) title ++ "║")
    drawMiddleBorder white panelWidth
    putStrLn $ white ("║" ++ padRight (panelWidth - 2) statusLine1 ++ "║")
    putStrLn $ white ("║" ++ padRight (panelWidth - 2) statusLine2 ++ "║")
    drawBottomBorder white panelWidth
    putStrLn ""

    printMapInBox panelWidth config state

    putStrLn ""
    drawTopBorder white panelWidth
    putStrLn $ white ("║" ++ padCenter (panelWidth - 2) help ++ "║")
    drawBottomBorder white panelWidth


-- | Imprime o mapa do jogo dentro de uma caixa com bordas.
-- | @param panelWidth Int: A largura total do painel para garantir o alinhamento.
-- | @param config GameConfig: A configuração do nível.
-- | @param state GameState: O estado do jogo.
-- | @return IO (): Uma ação de I/O que desenha a caixa do mapa.
printMapInBox :: Int -> GameConfig -> GameState -> IO ()
printMapInBox panelWidth config state = do
    let ((minY, minX), (maxY, maxX)) = A.bounds (gsMap state)
    let contentWidth = panelWidth - 2 

    drawTopBorder white panelWidth
    mapM_ (\y -> do
        let mapLine = [getCharForPos (y, x) config state | x <- [minX..maxX]]
        putStrLn $ white ("║" ++ padCenter contentWidth mapLine ++ "║")
        ) [minY..maxY]
    drawBottomBorder white panelWidth


-- | Função que determina qual caractere deve ser exibido para uma dada coordenada.
-- | @param pos (Int, Int): A coordenada (y, x) a ser avaliada.
-- | @param config GameConfig: A configuração do nível (para saber onde estão os alvos).
-- | @param state GameState: O estado do jogo (para saber a posição do jogador e das caixas).
-- | @return Char: O caractere a ser impresso para aquela posição.
getCharForPos :: (Int, Int) -> GameConfig -> GameState -> Char
getCharForPos pos config state
    | pos == gsPlayerPos state = 'P'
    | otherwise =
        let
            tile = (gsMap state) A.! pos
            isMark = pos `elem` (gcMarkPos config)
        in
            case tile of
                Wall -> '█'
                Box  -> if isMark then '*' else 'B'
                _    -> if isMark then 'x' else ' '


-- | Adiciona espaços à direita de uma string até que ela atinja um comprimento total desejado.
-- | @param totalLen Int: O comprimento final que a string deve ter.
-- | @param s String: A string original.
-- | @return String: A nova string com preenchimento.
padRight :: Int -> String -> String
padRight totalLen s = s ++ replicate (totalLen - length s) ' '


-- | Adiciona espaços em ambos os lados de uma string para centralizá-la dentro de um comprimento total.
-- | @param totalLen Int: O comprimento final da área onde a string será centralizada.
-- | @param s String: A string original.
-- | @return String: A nova string centralizada.
padCenter :: Int -> String -> String
padCenter totalLen s =
    let padding = totalLen - length s
        leftPad = padding `div` 2
        rightPad = padding - leftPad
    in replicate leftPad ' ' ++ s ++ replicate rightPad ' '
