{-# LANGUAGE DeriveGeneric #-}

import qualified Data.Array as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as Ae
import Data.Aeson (FromJSON, decode)
import GHC.Generics (Generic)
import Prelude
import System.Exit (exitFailure) -- Importar para sair do programa
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

-- === Tipos ===
data Tile = Wall | Floor | Box | Player | Mark deriving (Show, Eq)

data MapWrapper = MapWrapper { tileMap :: [String] }
    deriving (Show, Generic)

instance FromJSON MapWrapper

-- === Conversão entre Tile e Char ===
tileToChar :: Tile -> Char
tileToChar Wall   = '#'
tileToChar Floor  = ' '
tileToChar Box    = 'B'
tileToChar Player = '@'
tileToChar Mark   = 'X'

charToTile :: Char -> Tile
charToTile '#' = Wall
charToTile ' ' = Floor
charToTile 'B' = Box
charToTile '@' = Player
charToTile 'X' = Mark
charToTile _   = Floor

-- === Verificação de limites ===
inBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inBounds ((minY, minX), (maxY, maxX)) (y, x) =
    x >= minX && x <= maxX && y >= minY && y <= maxY

-- === Movimento do jogador ===
move :: Char -> (Int, Int) -> A.Array (Int, Int) Tile -> (Int, Int)
move tecla (y, x) gameMap =
    let newPos = case tecla of
                    'w' -> (y - 1, x)
                    's' -> (y + 1, x)
                    'a' -> (y, x - 1)
                    'd' -> (y, x + 1)
                    _   -> (y, x)
    in if inBounds (A.bounds gameMap) newPos
       then newPos
       else (y, x)

-- === Impressão do mapa ===
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

-- === Leitura do mapa JSON ===
loadMapFromJSON :: FilePath -> IO (A.Array (Int, Int) Tile)
loadMapFromJSON path = do
    content <- B.readFile path
    case decode content :: Maybe MapWrapper of
        Just (MapWrapper rows) -> do
            let tileLists = map (map charToTile) rows
                numRows = length tileLists
                numCols = if numRows > 0 then length (head tileLists) else 0
                assocs = [ ((y, x), tileLists !! y !! x) | y <- [0..numRows - 1], x <- [0..numCols - 1] ]
            return $ A.array ((0, 0), (numRows - 1, numCols - 1)) assocs
        Nothing -> do
            putStrLn "Falha ao decodificar o JSON! Verifique o formato."
            putStrLn $ "Conteúdo bruto lido (debug):\n" ++ show content
            exitFailure


-- === Loop do jogo ===
gameLoop :: A.Array (Int, Int) Tile -> (Int, Int) -> IO ()
gameLoop gameMap currentPlayerPos = do
    putStrLn "=== SOKOBAN ==="
    printMap gameMap currentPlayerPos
    putStrLn $ "Posição: " ++ show currentPlayerPos
    putStrLn "Use w/a/s/d para mover, q para sair"

    tecla <- getChar
    putStrLn ""

    if tecla == 'q'
        then putStrLn "Fim do jogo!"
        else do
            let newPos = move tecla currentPlayerPos gameMap
            if newPos == currentPlayerPos && tecla `elem` "wasd"
                then do
                    putStrLn "Movimento inválido!"
                    gameLoop gameMap currentPlayerPos
                else gameLoop gameMap newPos

-- === Início do jogo ===
start :: IO ()
start = do
    cwd <- getCurrentDirectory
    let jsonPath = cwd </> "data/maps/easy.json"
    gameMap <- loadMapFromJSON jsonPath
    gameLoop gameMap (4, 4)

-- === Main ===
main :: IO ()
main = start

