module Movement
  ( AbsoluteMovement(..)
  , Clock(..)
  , Move(..)
  , RelativeMovement(..)
  , Error(..)
  , applyMovement
  , clockMove
  , clockParser
  , dest
  , movements
  , movesDest
  , movesParser
  , movesPath
  , points
  , unparse
  ) where

import Lude

import Data.Array as Array
import Data.Array.NonEmpty (snoc')
import Data.Array.NonEmpty as Ne
import Data.Foldable (oneOf)
import Point (IPoint, Point(..))
import Point as Point
import StringParser (Parser)
import StringParser as Sp
import Syntax as Sntx

data Clock = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | C11 | C12

derive instance Eq Clock
derive instance Generic Clock _
instance Show Clock where
  show = genericShow

data Move
  = Step Clock
  | ToEdge Clock
  | ToZiggurat Clock
  | Reset

derive instance Eq Move
derive instance Generic Move _

instance Show Move where
  show = genericShow

unparse :: Move -> String
unparse = case _ of
  Step c -> unparseClock c
  ToEdge c -> unparseClock c <> fromChar Sntx.toEdge
  ToZiggurat c -> unparseClock c <> fromChar Sntx.toZig
  Reset -> fromChar Sntx.reset
  where
  unparseClock :: Clock -> String
  unparseClock = case _ of
    C1 -> "1"
    C2 -> "2"
    C3 -> "3"
    C4 -> "4"
    C5 -> "5"
    C6 -> "6"
    C7 -> "7"
    C8 -> "8"
    C9 -> "9"
    C10 -> fromChar Sntx._10
    C11 -> fromChar Sntx._11
    C12 -> fromChar Sntx._11

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
    snoc' [ p1, p2 ] destination

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

data Error
  = BelowEdge Move
  | InvalidContinuation Move
  | TooLowForZiggurat Move Int

movesPath
  :: ∀ @m
   . MonadError Error m
  => IPoint
  -> NonEmptyArray Move
  -> m (NonEmptyArray IPoint)
movesPath start = map snd <. foldl
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
              throwError $ BelowEdge move
        ToEdge c ->
          let
            y = Point.y pos
          in
            if y == 1 then
              pure $ pos /\ points'
            else if y < 1 then
              throwError $ BelowEdge move
            else
              let
                adjacentStep :: Clock -> m (IPoint /\ NonEmptyArray IPoint)
                adjacentStep clock = do
                  let am = applyMovement pos $ clockMove clock
                  toEdge <- movesPath (dest am) (pure $ ToEdge clock)
                  pure $ start /\ (points' <> toEdge)

                bridgeStep :: Clock -> m (IPoint /\ NonEmptyArray IPoint)
                bridgeStep clock = do
                  let am = applyMovement pos $ clockMove clock
                  toEdge <- movesPath (dest am) (pure $ ToEdge clock)
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
                  _ -> throwError $ InvalidContinuation move
        ToZiggurat c ->
          let
            y = Point.y pos
            f c1 c2 c3 =
              if y == 4 then do
                let am = applyMovement pos $ clockMove c2
                toZig <- movesPath (dest am) (pure $ ToEdge c3)
                pure $ Tuple
                  start
                  (Ne.appendArray points' (doubleThreat am) <> toZig)
              else do
                let am = applyMovement pos $ clockMove c1
                toZig <- movesPath (dest am) (pure $ ToZiggurat c1)
                pure $ Tuple
                  start
                  (Ne.appendArray points' (doubleThreat am) <> toZig)
          in
            if y < 4 then
              throwError $ TooLowForZiggurat move y
            else case c of
              C4 -> f C4 C5 C4
              C5 -> f C5 C5 C4
              C7 -> f C7 C7 C8
              C8 -> f C8 C7 C8
              _ -> throwError $ InvalidContinuation move
        Reset ->
          pure $ start /\ points'
  )
  (pure (start /\ pure start))

movesDest
  :: ∀ @m
   . MonadError Error m
  => IPoint
  -> NonEmptyArray Move
  -> m IPoint
movesDest = map Ne.last <.. movesPath

movesParser :: Parser (NonEmptyArray Move)
movesParser = do
  moves <- Ne.fromFoldable1 <$>
    Sp.many1 (Sp.try toEdgeParser <|> (Step <$> clockParser) <|> reset)
  oneOf
    [ Sp.char Sntx.sym $> reflect moves
    , pure moves
    ]
  where
  toEdgeParser :: Parser Move
  toEdgeParser = do
    clockParser >>= \clock ->
      (Sp.char Sntx.toEdge $> ToEdge clock)
        <|> (Sp.char Sntx.toZig $> ToZiggurat clock)

  reset :: Parser Move
  reset = Sp.char Sntx.reset $> Reset

  reflect :: NonEmptyArray Move -> NonEmptyArray Move
  reflect moves =
    let
      reflected = reflectMove <$> moves
    in
      if boundary (Ne.last moves) then
        moves <> reflected
      else
        Ne.snoc moves Reset <> reflected
    where
    reflectMove :: Move -> Move
    reflectMove = case _ of
      Step c -> Step $ reflectClock c
      ToEdge c -> ToEdge $ reflectClock c
      ToZiggurat c -> ToZiggurat $ reflectClock c
      m -> m
      where
      reflectClock :: Clock -> Clock
      reflectClock = case _ of
        C1 -> C11
        C2 -> C10
        C3 -> C9
        C4 -> C8
        C5 -> C7
        C7 -> C5
        C8 -> C4
        C9 -> C3
        C10 -> C2
        C11 -> C1
        c -> c

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
    <|> (Sp.char Sntx._10 $> C10)
    <|> (Sp.char Sntx._11 $> C11)
    <|> (Sp.char Sntx._12 $> C12)

movements :: NonEmptyArray Move -> Array (NonEmptyArray Move)
movements moves =
  foldl
    ( \(movements' /\ current) move ->
        if boundary move then
          let
            newMovement = Ne.prependArray current $ pure move
          in
            Array.snoc movements' newMovement /\ []
        else
          movements' /\ Array.snoc current move
    )
    ([] /\ [])
    moves
    # \(movements' /\ maybeMovement) ->
        case Ne.fromArray maybeMovement of
          Just movement -> Array.snoc movements' movement
          Nothing -> movements'

boundary :: Move -> Boolean
boundary = case _ of
  ToEdge _ -> true
  ToZiggurat _ -> true
  Reset -> true
  Step _ -> false
