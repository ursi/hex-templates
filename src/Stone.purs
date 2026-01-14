module Stone
  ( Connected(..)
  , Stone(..)
  , StoneMoves(..)
  , connected
  , disconnected
  , point
  , placeStone
  ) where

import Lude

import Movement (Move, movesDest)
import Point (IPoint)

newtype Connected = Connected Boolean

derive instance Newtype Connected _

connected :: IPoint -> Stone
connected = Stone $ Connected true

disconnected :: IPoint -> Stone
disconnected = Stone $ Connected false

data Stone = Stone Connected IPoint

data StoneMoves = StoneMoves Connected (NonEmptyArray Move)

placeStone
  :: ∀ @m
   . MonadError String m
  => IPoint
  -> StoneMoves
  -> m Stone
placeStone start (StoneMoves con moves) =
  Stone con <$> movesDest start moves

point :: Stone -> IPoint
point (Stone _ p) = p
