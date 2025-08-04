{-# LANGUAGE DeriveGeneric #-}

module Game.Types ( 
    Tile(..),
    MapWrapper(..),
    tileToChar,
    charToTile,
    FromJSON
) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data Tile = Wall | Floor | Box | Player | Mark deriving (Show, Eq)

data MapWrapper = MapWrapper { tileMap :: [String] } deriving (Show, Generic)

instance FromJSON MapWrapper

tileToChar :: Tile -> Char
tileToChar Wall   = '█'
tileToChar Floor  = ' '
tileToChar Box    = '≡'
tileToChar Player = '@'
tileToChar Mark   = 'x'


charToTile :: Char -> Tile
charToTile '█' = Wall
charToTile ' ' = Floor
charToTile '≡' = Box
charToTile '@' = Player
charToTile 'x' = Mark
