module Hexagon
  ( Hexagon(..)
  , Orientation(..)
  , circ
  , side
  , apo
  , ratio
  , gridPoint
  , vertices
  ) where

import MasonPrelude

import Data.Array.NonEmpty (NonEmptyArray, cons')
import Data.Number (cos, pi)
import Point (IPoint, NPoint, Point(..))

data Hexagon = Circ Number

ratio :: Number
ratio = 1.0 / cos (pi / 6.0)

apo :: Hexagon -> Number
apo (Circ r) = r / ratio

circ :: Hexagon -> Number
circ (Circ r) = r

side :: Hexagon -> Number
side = circ

gridPoint :: Hexagon -> IPoint -> NPoint
gridPoint h (Point x y) =
  Point
    (apo h + 2.0 * apo h * toNumber (x - 1) + apo h * toNumber (y - 1))
    (circ h + (1.5 * side h) * toNumber (y - 1))

data Orientation
  = Tall
  | Wide

vertices :: Orientation -> Hexagon -> NonEmptyArray NPoint
vertices orientation h =
  let
    a = apo h
    r = circ h
    s = side h / 2.0
  in
    case orientation of
      Tall ->
        cons'
          (Point 0.0 r)
          [ Point a s
          , Point a (-s)
          , Point 0.0 (-r)
          , Point (-a) (-s)
          , Point (-a) s
          ]
      Wide ->
        cons'
          (Point r 0.0)
          [ Point s a
          , Point (-s) a
          , Point (-r) 0.0
          , Point (-s) (-a)
          , Point s (-a)
          ]
