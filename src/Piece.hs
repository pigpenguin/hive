module Piece where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

data PColor = Black | White deriving Eq
data PSpecies = Bee | GrassHopper | Ant | Beetle | Spider

otherColor :: PColor -> PColor
otherColor Black = White
otherColor White = Black

speciesToText :: PSpecies -> String
speciesToText Bee         = "Q"
speciesToText GrassHopper = "G"
speciesToText Ant         = "A"
speciesToText Beetle      = "B"
speciesToText Spider      = "S"

samplePiece :: Piece
samplePiece = Piece {color = Black, species = Bee}

hWidth :: Double
hWidth = 2

hRadius :: Double
hRadius = hWidth/2

hHeight :: Double
hHeight = sqrt 3 / 2 * hWidth

hex :: Diagram B
hex = regPoly 6 hRadius

data Piece = Piece { color   :: PColor
                   , species :: PSpecies}

renderPiece :: Piece -> Diagram B
renderPiece piece = text char # fc (col . otherColor $ colour) <> hex # fc (col colour) # lc grey
    where 
        char = speciesToText $ species piece
        colour = color piece
        col c
            | c == Black = black
            | otherwise = white
