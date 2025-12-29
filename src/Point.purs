module Point where

import MasonPrelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Ne
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

instance CommutativeRing a => CommutativeRing (Point a)

instance DivisionRing a => DivisionRing (Point a) where
  recip (Point x' y') = Point (recip x') (recip y')

instance Field a => EuclideanRing (Point a) where
  degree _ = 1
  div (Point x1 y1) (Point x2 y2) = Point (x1 / x2) (y1 / y2)
  mod (Point x1 y1) (Point x2 y2) = Point (mod x1 x2) (mod y1 y2)

x :: ∀ a. Point a -> a
x (Point x' _) = x'

y :: ∀ a. Point a -> a
y (Point _ y') = y'

midpoint :: ∀ a. EuclideanRing a => Point a -> Point a -> Point a
midpoint (Point x1 y1) (Point x2 y2) =
  let
    two = one + one
  in
    Point ((x1 + x2) / two) ((y1 + y2) / two)

keepMin :: ∀ a. Ord a => Point a -> Point a -> Point a
keepMin (Point x1 y1) (Point x2 y2) = Point (min x1 x2) (min y1 y2)

keepMax :: ∀ a. Ord a => Point a -> Point a -> Point a
keepMax (Point x1 y1) (Point x2 y2) = Point (max x1 x2) (max y1 y2)

type Box a =
  { min :: Point a
  , max :: Point a
  }

box :: ∀ a. Ord a => Semiring a => NonEmptyArray (Point a) -> Box a
box points =
  foldl
    ( \b point ->
        { min: keepMin b.min point
        , max: keepMax b.max point
        }
    )
    { min: Ne.head points, max: Ne.head points }
    (Ne.tail points)

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
arc length' angle = rotate angle (Point length' 0.0)

rotate :: Number -> NPoint -> NPoint
rotate angle (Point x' y') =
  Point
    (x' * cos angle - y' * sin angle)
    (x' * sin angle + y' * cos angle)
