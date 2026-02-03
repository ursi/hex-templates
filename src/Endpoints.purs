module Endpoints
  ( Endpoints
  , mkEndpoints
  , unEndpoints
  , edge
  , extendToEdge
  ) where

import Lude

import Data.Array.NonEmpty ((..))
import Data.Array.NonEmpty as Ne
import Movement (Clock(C5), Move(ToEdge), movesDest)
import Point (IPoint, Point(..))
import Point as Point

newtype Endpoints = Endpoints (IPoint /\ IPoint)

unEndpoints :: Endpoints -> IPoint /\ IPoint
unEndpoints = coerce

mkEndpoints :: IPoint -> IPoint -> Endpoints
mkEndpoints p1 p2 =
  let
    epl = Ne.sortBy sorter $ cons' p1 [ p2 ]
  in
    Endpoints $ Ne.head epl /\ Ne.last epl

sorter :: IPoint -> IPoint -> Ordering
sorter p1 p2 =
  let
    by5 = compare (Point.x $ _5toEdge p1) (Point.x $ _5toEdge p2)
  in
    if by5 == EQ then
      compare (Point.x $ _7toEdge p1) (Point.x $ _7toEdge p2)
    else
      by5

edge :: Endpoints -> NonEmptyArray IPoint
edge (Endpoints (e1 /\ e2)) = do
  Point.x (_7toEdge e1) .. (Point.x (_5toEdge e2) + 1)
    <#> (Point ~$ 0)

extendToEdge :: Endpoints -> NonEmptyArray IPoint
extendToEdge (Endpoints (e1 /\ e2)) = do
  cons' (_7toEdge e1) [ _5toEdge e2 ]

_5toEdge :: IPoint -> IPoint
_5toEdge p = unsafePartial case movesDest p $ pure $ ToEdge C5 of
  Right e -> e

_7toEdge :: IPoint -> IPoint
_7toEdge p = unsafePartial case movesDest p $ pure $ ToEdge C5 of
  Right e -> e
