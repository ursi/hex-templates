module Hexagon
  ( Hexagon
  , fromWidth
  , side
  , cap
  , height
  , width
  , gridPoint
  -- , gridPointB
  ) where

import MasonPrelude

import Data.Number (cos, pi)
import Point (IPoint, NPoint, Point(..))

-- a hexagon characterized by it's inscribed diameter
newtype Hexagon = Hexagon Number

fromWidth :: Number -> Hexagon
fromWidth = Hexagon

side :: Hexagon -> Number
side (Hexagon w) = w / 2.0 / cos (pi / 6.0)

cap :: Hexagon -> Number
cap h = side h / 2.0

height :: Hexagon -> Number
height h = 2.0 * side h

width :: Hexagon -> Number
width = coerce

gridPoint :: Hexagon -> IPoint -> NPoint
gridPoint h (Point x y) =
  Point
    (width h / 2.0 + width h * toNumber (x - 1) + width h / 2.0 * toNumber (y - 1))
    (height h / 2.0 + (cap h + side h) * toNumber (y - 1))

