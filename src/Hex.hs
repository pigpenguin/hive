{-# LANGUAGE FlexibleInstances #-}
module Hex where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Piece

cell :: (Int, Int) -> Piece -> (P2 Double, Diagram B)
cell (r,q) piece = (p2 (x,y), renderPiece piece)
    where
        x = hRadius * 3/2 * fromIntegral q
        y = hRadius * sqrt 3 * (fromIntegral r 
                           + fromIntegral q/2)

type HDirection = (Int, Int)

up :: HDirection
up = (1,0)

down :: HDirection
down = -1 * up

upRight :: HDirection
upRight = (0,1)

downLeft :: HDirection
downLeft = -1 * upRight

downRight :: HDirection
downRight = down + upRight

upLeft :: HDirection
upLeft = up + downLeft

directions :: [HDirection]
directions = [up, upRight, downRight, down, downLeft, upLeft]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors = flip map directions . (+)

instance Num ((Int, Int)) where
    negate (x,y) = (-x,-y)
    (+) (x,y) (a,b) = (x+a,y+b)
    (*) (x,y) (a,b) = (x*a,y*b)
    fromInteger n = (i,i)
        where
            i = fromIntegral n
    abs (x,y) = (abs x, abs y)
    signum = undefined
