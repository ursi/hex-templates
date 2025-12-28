module Point where

import MasonPrelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Number (cos, sin, sqrt)

data Point a = Point a a
type NPoint = Point Number
type IPoint = Point Int

derive instance Eq a => Eq (Point a)
derive instance Ord a => Ord (Point a)

instance Show a => Show (Point a) where
  show (Point x' y') = "Point " <> show x' <> " " <> show y'

instance Semiring a => Semiring (Point a) where
  add (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)
  zero = Point zero zero
  mul (Point x1 y1) (Point x2 y2) = Point (x1 * x2) (y1 * y2)
  one = Point one one

instance Ring a => Ring (Point a) where
  sub (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

x :: ∀ a. Point a -> a
x (Point x' _) = x'

y :: ∀ a. Point a -> a
y (Point _ y') = y'

midpoint :: NPoint -> NPoint -> NPoint
midpoint (Point x1 y1) (Point x2 y2) =
  Point ((x1 + x2) / 2.0) ((y1 + y2) / 2.0)

type Box a =
  { min :: Point a
  , max :: Point a
  }

box :: ∀ a. Ord a => Semiring a => NonEmptyArray (Point a) -> Box a
box points =
  let
    max' =
      Point
        (foldl max zero $ x <$> points)
        (foldl max zero $ y <$> points)
    min' =
      Point
        (foldl min (x max') $ x <$> points)
        (foldl min (y max') $ y <$> points)
  in
    { min: min', max: max' }

reflect :: ∀ a. Ring a => Point a -> Point a -> Point a
reflect (Point x1 y1) (Point x2 y2) =
  let
    two = one + one
  in
    Point
      (x1 + two * (x2 - x1))
      (y1 + two * (y2 - y1))

length :: NPoint -> Number
length (Point x' y') = sqrt (x' ^ 2.0 + y' ^ 2.0)

arc :: Number -> Number -> NPoint
arc length' angle =
  Point (length' * cos angle) (length' * sin angle)

rotate :: Number -> NPoint -> NPoint
rotate angle (Point x' y') =
  Point
    (x' * cos angle - y' * sin angle)
    (x' * sin angle + y' * cos angle)
