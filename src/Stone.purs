module Stone
  ( Stone
  , connected
  , disconnected
  , placeStones
  ) where

import Lude

import Data.Array.NonEmpty as Ne
import Movement (Clock, applyMovement, clockMove, dest)
import Point (IPoint)
import Point as Point

newtype Connected = Connected Boolean

derive instance Newtype Connected _

connected :: IPoint -> Stone
connected = { connected: true, pos: _ }

disconnected :: IPoint -> Stone
disconnected = { connected: false, pos: _ }

type Stone =
  { connected :: Boolean
  , pos :: IPoint
  }

type StoneMoves =
  { reset :: Boolean
  , connected :: Boolean
  , moves :: NonEmptyArray Clock
  }

placeStones
  :: ∀ @m
   . MonadError String m
  => IPoint
  -> NonEmptyArray StoneMoves
  -> m (NonEmptyArray Stone)
placeStones start stonemoves =
  foldl
    ( \acc { reset, connected: connected', moves } -> do
        stones <- acc
        newStonePoint <-
          if reset then
            clockDest start moves
          else
            clockDest (Ne.last stones).pos moves
        pure $ Ne.snoc stones $ { connected: connected', pos: newStonePoint }
    )
    (pure $ pure $ connected start)
    stonemoves

clockDest :: ∀ @m. MonadError String m => IPoint -> NonEmptyArray Clock -> m IPoint
clockDest start = foldl
  ( \acc c -> do
      pos <- acc
      let newPos = dest $ applyMovement pos $ clockMove c
      if Point.y newPos >= 1 then
        pure $ newPos
      else
        throwError $ "`" <> show c <> "` moves you below the edge"
  )
  (pure start)
