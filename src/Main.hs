{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.List (nub)

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Hex
import Piece

blackSpider :: Piece
blackSpider = Piece {color = Black, species = Spider}

whiteSpider :: Piece
whiteSpider = Piece {color = White, species = Spider}

blackQueen :: Piece
blackQueen = Piece {color = Black, species = Bee}

whiteQueen :: Piece
whiteQueen = Piece {color = White, species = Bee}

whiteAnt :: Piece
whiteAnt = Piece {color = White, species = Ant}

pieces :: [Piece]
pieces = cycle [blackSpider, whiteSpider, whiteAnt]

hexagon' :: Int -> [(Int, Int)]
hexagon' 0 = [(0,0)]
hexagon' n = nub $ concatMap neighbors (hexagon' $ n-1)

main :: IO()
main = mainWith $ position (zipWith cell (hexagon' 8) pieces)
