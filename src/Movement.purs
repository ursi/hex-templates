module Movement
  ( Clock(..)
  , Move(..)
  , RelativeMovement(..)
  , AbsoluteMovement(..)
  , applyMovement
  , dest
  , points
  , clockMove
  , clockPath
  , movesParser
  ) where

import MasonPrelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array.NonEmpty (NonEmptyArray, cons')
import Data.Array.NonEmpty as Ne
import Point (IPoint, Point(..))
import Point as Point
import StringParser (Parser)
import StringParser as Sp

data Clock = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | C11 | C12

derive instance Generic Clock _
instance Show Clock where
  show = genericShow

data Move
  = Step Clock
  | ToEdge Clock
  | ToZiggurat Clock
  | Reset

derive instance Generic Move _

instance Show Move where
  show = genericShow

data RelativeMovement
  = Single IPoint
  | Bridge
      { destination :: IPoint
      , doubleThreat :: IPoint /\ IPoint
      }

dest :: AbsoluteMovement -> IPoint
dest (AbsoluteMovement m) = case m of
  Single p -> p
  Bridge { destination } -> destination

points :: AbsoluteMovement -> NonEmptyArray IPoint
points (AbsoluteMovement m) = case m of
  Single p -> pure p
  Bridge { destination, doubleThreat: p1 /\ p2 } ->
    cons' destination [ p1, p2 ]

doubleThreat :: AbsoluteMovement -> Array IPoint
doubleThreat (AbsoluteMovement m) = case m of
  Single _ -> []
  Bridge { doubleThreat: p1 /\ p2 } -> [ p1, p2 ]

newtype AbsoluteMovement = AbsoluteMovement RelativeMovement

clockMove :: Clock -> RelativeMovement
clockMove = case _ of
  C1 -> Single $ Point 0 1
  C2 -> Bridge
    { destination: Point 1 1
    , doubleThreat: Point 0 1 /\ Point 1 0
    }
  C3 -> Single $ Point 1 0
  C4 -> Bridge
    { destination: Point 2 (-1)
    , doubleThreat: Point 1 0 /\ Point 1 (-1)
    }
  C5 -> Single $ Point 1 (-1)
  C6 -> Bridge
    { destination: Point 1 (-2)
    , doubleThreat: Point 1 (-1) /\ Point 0 (-1)
    }
  C7 -> Single $ Point 0 (-1)
  C8 -> Bridge
    { destination: Point (-1) (-1)
    , doubleThreat: Point 0 (-1) /\ Point (-1) 0
    }
  C9 -> Single $ Point (-1) 0
  C10 -> Bridge
    { destination: Point (-2) 1
    , doubleThreat: Point (-1) 0 /\ Point (-1) 1
    }
  C11 -> Single $ Point (-1) 1
  C12 -> Bridge
    { destination: Point (-1) 2
    , doubleThreat: Point 0 1 /\ Point (-1) 1
    }

applyMovement :: IPoint -> RelativeMovement -> AbsoluteMovement
applyMovement p m = AbsoluteMovement case m of
  Single p' -> Single $ p + p'
  Bridge b ->
    Bridge
      { destination: p + b.destination
      , doubleThreat: (p /\ p) + b.doubleThreat
      }

clockPath
  :: ∀ @m
   . MonadError String m
  => IPoint
  -> NonEmptyArray Move
  -> m (NonEmptyArray IPoint)
clockPath start = map snd <. foldl
  ( \acc move -> do
      pos /\ points' <- acc
      case move of
        Step c ->
          let
            am = applyMovement pos $ clockMove c
            newPos = dest am
          in
            if Point.y newPos >= 1 then
              pure $ newPos /\ (points' <> points am)
            else
              throwError $ "`" <> show (Step c) <> "` moves you below the edge"
        ToEdge c ->
          let
            y = Point.y pos
          in
            if y == 1 then
              pure $ pos /\ points'
            else if y < 1 then
              throwError "you are below the edge"
            else
              let
                adjacentStep :: Clock -> m (IPoint /\ NonEmptyArray IPoint)
                adjacentStep clock = do
                  let am = applyMovement pos $ clockMove clock
                  toEdge <- clockPath (dest am) (pure $ ToEdge clock)
                  pure $ start /\ (points' <> toEdge)

                bridgeStep :: Clock -> m (IPoint /\ NonEmptyArray IPoint)
                bridgeStep clock = do
                  let am = applyMovement pos $ clockMove clock
                  toEdge <- clockPath (dest am) (pure $ ToEdge clock)
                  pure $ Tuple
                    start
                    (Ne.appendArray points' (doubleThreat am) <> toEdge)
              in
                case c of
                  C4 ->
                    if y == 2 then
                      let
                        am = applyMovement pos $ clockMove C5
                      in
                        pure $ start /\ (points' <> points am)
                    else
                      bridgeStep C4
                  C5 -> adjacentStep C5
                  C6 ->
                    if y == 2 then
                      let
                        am5 = applyMovement pos $ clockMove C5
                        am7 = applyMovement pos $ clockMove C7
                      in
                        pure $ start /\ (points' <> points am5 <> points am7)
                    else
                      bridgeStep C6
                  C7 -> adjacentStep C7
                  C8 ->
                    if y == 2 then
                      let
                        am = applyMovement pos $ clockMove C7
                      in
                        pure $ start /\ (points' <> points am)
                    else do
                      bridgeStep C8
                  _ -> throwError $ "`" <> show move <> "` is an invlid move"
        ToZiggurat c ->
          let
            y = Point.y pos
            f c1 c2 c3 =
              if y == 4 then do
                let am = applyMovement pos $ clockMove c2
                toZig <- clockPath (dest am) (pure $ ToEdge c3)
                pure $ Tuple
                  start
                  (Ne.appendArray points' (doubleThreat am) <> toZig)
              else do
                let am = applyMovement pos $ clockMove c1
                toZig <- clockPath (dest am) (pure $ ToZiggurat c1)
                pure $ Tuple
                  start
                  (Ne.appendArray points' (doubleThreat am) <> toZig)
          in
            if y < 4 then
              throwError $ "you cannot ziggurat to the edge from height " <> show y
            else case c of
              C4 -> f C4 C5 C4
              C5 -> f C5 C5 C4
              C7 -> f C7 C7 C8
              C8 -> f C8 C7 C8
              _ -> throwError $ "`" <> show move <> "` is an invlid move"
        Reset ->
          pure $ start /\ points'
  )
  (pure (start /\ pure start))

movesParser :: Parser (NonEmptyArray Move)
movesParser = Ne.fromFoldable1 <$>
  Sp.many1 (Sp.try toEdgeParser <|> (Step <$> clockParser) <|> reset)
  where
  clockParser :: Parser Clock
  clockParser =
    ( Sp.anyDigit >>= \d -> case d of
        '1' -> pure C1
        '2' -> pure C2
        '3' -> pure C3
        '4' -> pure C4
        '5' -> pure C5
        '6' -> pure C6
        '7' -> pure C7
        '8' -> pure C8
        '9' -> pure C9
        _ -> Sp.fail "only the digits 1-9 can be used"
    )
      <|> (Sp.char 'a' $> C10)
      <|> (Sp.char 'b' $> C11)
      <|> (Sp.char 'c' $> C12)

  toEdgeParser :: Parser Move
  toEdgeParser = do
    clockParser >>= \clock ->
      (Sp.char '.' $> ToEdge clock)
        <|> (Sp.char 'z' $> ToZiggurat clock)

  reset :: Parser Move
  reset = Sp.char '*' $> Reset
