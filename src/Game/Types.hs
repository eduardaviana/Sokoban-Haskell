-- | Habilita a extensão 'DeriveGeneric' para a derivação automática de instâncias.
{-# LANGUAGE DeriveGeneric #-}

-- | Módulo que define os tipos de dados fundamentais para o jogo
module Game.Types where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

-- | Define um tipo de dado chamado 'Tile', que pode ser um de cinco valores possíveis: 'Wall' (parede), 'Floor' (chão), 'Box' (caixa), 'Player' (jogador) ou 'Mark' (marca).
-- | 'deriving (Show, Eq)' automaticamente adiciona funcionalidades:
-- |   - 'Show': Permite que os valores de 'Tile' sejam convertidos em strings 
-- |   - 'Eq': Permite que os valores de 'Tile' sejam comparados para igualdade ('==') ou desigualdade ('/=').
data Tile = Wall | Floor | Box | Player | Mark deriving (Show, Eq)

-- | Define um tipo de dado: 'MapWrapper' para a leitura de JSON.
data MapWrapper = MapWrapper { tileMap :: [String] } deriving (Show, Generic)

-- | Declara que o tipo 'MapWrapper' é uma instância da classe de tipo 'FromJSON'.
instance FromJSON MapWrapper

-- | Função que converte um valor do tipo 'Tile' para um 'Char' correspondente, para exibição no terminal.
-- | @param tile Tile: O tipo de tile a ser convertido.
-- | @return Char: O caractere que representa o tile no terminal.
tileToChar :: Tile -> Char
tileToChar Wall   = '#'
tileToChar Floor  = ' '
tileToChar Box    = 'B'
tileToChar Player = '@'
tileToChar Mark   = 'x'


-- | Função que converte um 'Char' de volta para um valor do tipo 'Tile'.
-- | @param char Char: O caractere a ser convertido.
-- | @return Tile: O tile correspondente.
charToTile :: Char -> Tile
charToTile '#' = Wall
charToTile ' ' = Floor
charToTile 'B' = Box
charToTile '@' = Player
charToTile 'x' = Mark
charToTile _   = Floor
