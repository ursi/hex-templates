module Stone
  ( Stone
  , connected
  , disconnected
  , placeStones
  , placeEnemyStones
  , stonesParser
  , manyStoneMovesParser
  ) where

import Lude

import Data.Array as Array
import Data.Array.NonEmpty as Ne
import Movement (Clock, applyMovement, clockMove, clockParser, dest)
import Point (IPoint)
import Point as Point
import StringParser (Parser)
import StringParser as Sp

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
  -> Array StoneMoves
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

placeEnemyStones
  :: ∀ @m
   . MonadError String m
  => IPoint
  -> Array StoneMoves
  -> m (Array IPoint)
placeEnemyStones start stonemoves =
  foldl
    ( \acc { reset, moves } -> do
        stones <- acc
        newStonePoint <-
          if reset then
            clockDest start moves
          else
            clockDest (fromMaybe start $ Array.last stones) moves
        pure $ Array.snoc stones $ newStonePoint
    )
    (pure [])
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

stonesParser :: Parser { height :: Int, stoneMoves :: Array StoneMoves }
stonesParser = do
  height <- unsafeParseInt <$> Sp.regex "\\d+"
  stoneMoves <- (Sp.char ':' *> manyStoneMovesParser) <|> pure []
  pure { height, stoneMoves }

manyStoneMovesParser :: Parser (Array StoneMoves)
manyStoneMovesParser = Array.fromFoldable <$> Sp.sepEndBy stoneMovesParser (Sp.char ':')
  where
  stoneMovesParser :: Parser StoneMoves
  stoneMovesParser = do
    reset <- (Sp.char '*' $> true) <|> pure false
    moves <- Ne.fromFoldable1 <$> Sp.many1 clockParser
    connected' <- (Sp.char '^' $> true) <|> pure false
    pure { reset, connected: connected', moves }
