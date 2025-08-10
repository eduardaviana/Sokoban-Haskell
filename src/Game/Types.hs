-- | Habilita a extensão 'DeriveGeneric' para a derivação automática de instâncias.
{-# LANGUAGE DeriveGeneric #-}

-- | Módulo que define os tipos de dados fundamentais para o jogo
module Game.Types where

import qualified Data.Array as A
import GHC.Generics (Generic) 
import Data.Aeson (FromJSON)


-- | Define um tipo de dado chamado Tile, que pode ser um de cinco valores possíveis: Wall, Floor, Box, Player ou Mark, que são os elementos do jogo.
-- | deriving (Show, Eq) automaticamente adiciona funcionalidades:
-- | Show: Permite que os valores de 'Tile' sejam convertidos em strings 
-- | Eq: Permite que os valores de 'Tile' sejam comparados para igualdade ('==') ou desigualdade ('/=').
data Tile = Wall | Floor | Box | Player | Mark deriving (Show, Eq)

-- | Representa as direções de movimento possíveis para o jogador.
data Direction = Up | Down | GoLeft | GoRight deriving (Eq, Show)

-- | Representa as ações que o jogador pode realizar durante o jogo.
data Action = Move Direction | Restart | Quit | Undo | NoOp deriving (Eq, Show)

-- | Define a configuração de um nível. Contém dados que não mudam durante uma partida.
data GameConfig = GameConfig
  { gcDifficulty :: String, 
    gcLevel      :: Int, 
    gcMarkPos    :: [(Int, Int)]
  } deriving (Show)


-- | Define o estado do jogo. Contém dados que mudam a cada movimento do jogador.
data GameState = GameState
  { gsMap       :: A.Array (Int, Int) Tile, 
    gsPlayerPos :: (Int, Int), 
    gsMoveCount   :: Int,
    gsHistory     :: [GameState]
  } 


-- | Tipo de dado auxiliar para a decodificação de um único nível do arquivo JSON.
data LevelLocal = LevelLocal
  { tileMap :: [String]
  , marks   :: [[Int]]
  } deriving (Show, Generic)


-- | Tipo de dado auxiliar que representa a estrutura completa do arquivo JSON, contendo uma lista de níveis.
data MapWrapperLocal = MapWrapperLocal
  { levels :: [LevelLocal]
  } deriving (Show, Generic)


-- | Declara que o tipo 'LevelLocal' é uma instância da classe 'FromJSON'.
instance FromJSON LevelLocal


-- | Declara que o tipo 'MapWrapperLocal' é uma instância da classe 'FromJSON'.
instance FromJSON MapWrapperLocal


-- | Função que converte um valor do tipo 'Tile' para um 'Char' correspondente, para exibição no terminal.
-- | @param Tile: O tipo de tile a ser convertido.
-- | @return Char: O caractere que representa o tile no terminal.
tileToChar :: Tile -> Char
tileToChar Wall   = '#'
tileToChar Floor  = ' '
tileToChar Box    = 'B'
tileToChar Player = '@'
tileToChar Mark   = 'x'


-- | Função que converte um 'Char' de volta para um valor do tipo 'Tile'.
-- | @param Char: O caractere a ser convertido.
-- | @return Tile: O tile correspondente.
charToTile :: Char -> Tile
charToTile '#' = Wall
charToTile ' ' = Floor
charToTile 'B' = Box
charToTile '@' = Player
charToTile 'x' = Mark
charToTile _   = Floor
