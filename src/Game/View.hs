-- | Módulo responsável por toda a renderização do jogo no console.
-- | Ele é uma camada de "apresentação" que apenas lê o estado do jogo e o exibe.
module Game.View (render) where

import qualified Data.Array as A
import System.FilePath (takeBaseName)
import Game.Types
import Game.Utils (drawBottomBorder, drawMiddleBorder, drawTopBorder, clearScreen, capitalize)


-- | Função principal de renderização. Orquestra a limpeza e o desenho da tela completa do jogo.
-- | @param config GameConfig: A configuração imutável do nível (para exibir dificuldade, nível, etc.).
-- | @param state GameState: O estado atual e mutável do jogo (mapa, posição do jogador).
-- | @return IO (): Uma ação de I/O que desenha o jogo no terminal.
render :: GameConfig -> GameState -> IO ()
render config state = do
    clearScreen

    let titulo = "SOKOBAN"
        nivel = "Nível: " ++ show (gcLevel config + 1)
        dificuldade = "Dificuldade: " ++ capitalize (takeBaseName (gcDifficulty config))
        statusLinha1 = nivel ++ " | " ++ dificuldade

        totalCaixas = length (gcMarkPos config)
        caixasNoLugar = length $ filter (\p -> (gsMap state) A.! p == Box) (gcMarkPos config)
        statusCaixas = "Caixas no Alvo: " ++ show caixasNoLugar ++ "/" ++ show totalCaixas
        statusMovimentos = "Movimentos: " ++ show (gsMoveCount state)
        statusLinha2 = statusCaixas ++ " | " ++ statusMovimentos

        ajuda = "[W/A/S/D] Mover | [R] Reiniciar | [Q] Sair"

    let ((_, minX), (_, maxX)) = A.bounds (gsMap state)
        mapWidth = maxX - minX + 1
        contentWidth = maximum [mapWidth, length statusLinha1, length statusLinha2, length ajuda, length titulo]
        panelWidth = contentWidth + 4 

    drawTopBorder panelWidth
    putStrLn $ "║" ++ padCenter (panelWidth - 2) titulo ++ "║"
    drawMiddleBorder panelWidth
    putStrLn $ "║" ++ padRight (panelWidth - 2) statusLinha1 ++ "║"
    putStrLn $ "║" ++ padRight (panelWidth - 2) statusLinha2 ++ "║"
    drawBottomBorder panelWidth
    putStrLn ""

    printMapInBox panelWidth config state

    putStrLn ""
    drawTopBorder panelWidth
    putStrLn $ "║" ++ padCenter (panelWidth - 2) ajuda ++ "║"
    drawBottomBorder panelWidth

-- | Imprime o mapa do jogo dentro de uma caixa com bordas.
-- | @param panelWidth Int: A largura total do painel para garantir o alinhamento.
-- | @param config GameConfig: A configuração do nível.
-- | @param state GameState: O estado do jogo.
-- | @return IO (): Uma ação de I/O que desenha a caixa do mapa.
printMapInBox :: Int -> GameConfig -> GameState -> IO ()
printMapInBox panelWidth config state = do
    let ((minY, minX), (maxY, maxX)) = A.bounds (gsMap state)
    let contentWidth = panelWidth - 2 -- Área útil dentro das bordas

    drawTopBorder panelWidth
    mapM_ (\y -> do
        let mapLine = [getCharForPos (y, x) config state | x <- [minX..maxX]]
        putStrLn $ "║" ++ padCenter contentWidth mapLine ++ "║"
        ) [minY..maxY]
    drawBottomBorder panelWidth


-- | Função pura que determina qual caractere deve ser exibido para uma dada coordenada.
-- | A lógica de exibição (cores, símbolos) é centralizada aqui.
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