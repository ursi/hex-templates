module Main where

import Lude

import Data.Array as Array
import Data.Array.NonEmpty ((..))
import Data.Array.NonEmpty as Ne
import Data.Bifunctor (lmap)
import Data.Foldable (elem, oneOf)
import Data.Semigroup.Foldable (class Foldable1, fold1, foldl1)
import Deku.Core (Nut, fixed)
import Deku.DOM (text_)
import Deku.DOM as D
import Deku.DOM.Attributes as A
import Deku.DOM.Listeners as L
import Deku.DOM.SVG as Svg
import Deku.DOM.SVG.Attributes as SvgA
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Deku.Toplevel (runInBody)
import FRP.Poll (Poll)
import Hexagon (Hexagon(Circ), Orientation(Tall))
import Hexagon as Hex
import Magic (magic)
import Movement
  ( Clock(C11, C9, C7, C5, C3, C1)
  , Endpoints
  , Move
  , applyMovement
  , clockMove
  , dest
  , edge
  , mkEndpoints
  , movements
  , movesDest
  , movesParser
  , movesPath
  , unEndpoints
  )
import Point (Box, IPoint, NPoint, Point(..))
import Point as Point
import Stone (Stone, manyStoneMovesParser, stonesParser)
import Stone as Stone
import StringParser (Parser, printParserError, runParser)
import StringParser as Sp

main :: Effect Unit
main = do
  void $ runInBody Deku.do
    set /\ poll <- magic { stepsStr: useState "3-4.7." }
    let
      svgDataP :: Poll (Either String SvgData)
      svgDataP = poll.stepsStr <#> \stepsStr ->
        if stepsStr == "" then do
          pure
            { stones: pure $ Stone.connected $ Point 1 1
            , mcarrier: Just
                { cells: pure $ Point 1 1
                , endpoints: mkEndpoints (Point 1 1) (Point 1 1)
                }
            , enemyStones: []
            }
        else do
          { stones, carrierMoves, enemyStones } <- lmap printParserError
            $ runParser templateSpecParser stepsStr
          case Ne.fromArray carrierMoves of
            Just carrierMovesNe -> do
              let start = (Ne.head stones).pos
              carrier <- movesPath start carrierMovesNe
              endpoints <- case movements carrierMovesNe of
                [ m ] -> do
                  endpoint <- movesDest start m
                  pure $ mkEndpoints start endpoint
                [ m1, m2 ] -> do
                  endpoint1 <- movesDest start m1
                  endpoint2 <- movesDest start m2
                  pure $ mkEndpoints endpoint1 endpoint2
                ms ->
                  throwError
                    $ "You cannot have more than 2 movements, and you have "
                    <> show (Array.length ms)
                    <> "."
              pure
                { stones
                , mcarrier: Just
                    { cells: fill { cells: Ne.nub carrier, endpoints }
                    , endpoints
                    }
                , enemyStones
                }
            Nothing ->
              pure
                { stones
                , mcarrier: Nothing
                , enemyStones: []
                }

    D.div
      [ A.style_
          """
          height: 100vh;
          display: flex;
          justify-content: center;
          """
      ]
      [ inputs set poll
      , hexagonSvgs svgDataP
      ]
  where
  inputs set poll =
    D.div [ A.style_ "position: absolute; z-index: 1" ]
      [ D.input [ A.value poll.stepsStr, L.valueOn_ L.input set.stepsStr ] [] ]

type CarrierData =
  { cells :: NonEmptyArray IPoint
  , endpoints :: Endpoints
  }

type SvgData =
  { stones :: NonEmptyArray Stone
  , mcarrier :: Maybe CarrierData
  , enemyStones :: Array IPoint
  }

hexagonSvgs :: Poll (Either String SvgData) -> Nut
hexagonSvgs svgDataP =
  svgDataP <#~> case _ of
    Right { mcarrier, stones, enemyStones } ->
      case mcarrier of
        Just carrier ->
          let
            edgePoints = edge carrier.endpoints
          in
            svgContent
              (carrier.cells <> edgePoints <> (stones <#> _.pos))
              $ fixed
                  [ fixed $ Array.fromFoldable $ placeHexagon <$> carrier.cells
                  , fixed $ Array.fromFoldable $ placeEdge <$> edgePoints
                  , fixed $ Array.fromFoldable $ placeStones <$> stones
                  , fixed $ Array.fromFoldable $ placeEnemyStones <$> enemyStones
                  ]
        Nothing ->
          svgContent
            (stones <#> _.pos)
            (fixed $ Array.fromFoldable $ placeStones <$> stones)
    Left error -> D.div
      [ A.style_
          """
          height: 100%;
          display: flex;
          justify-content: center;
          align-items: center;
          """
      ]
      [ text_ error ]
  where
  svgContent :: NonEmptyArray IPoint -> Nut -> Nut
  svgContent allPoints content =
    Svg.svg
      [ SvgA.width_ "90%"
      , SvgA.height_ "80%"
      , SvgA.viewBox_ $ makeViewBox allPoints
      , SvgA.transform_ "scale(1,-1)"
      , SvgA.preserveAspectRatio_ "xMidYMin"
      ]
      [ Svg.defs_
          ( -- organized like this for readability
            [ hexSvgId
                /\ \id -> Svg.polygon
                  [ SvgA.id_ id
                  , SvgA.strokeWidth_ $ show strokeWidth
                  , SvgA.stroke_ "black"
                  , SvgA.fill_ "rgb(214, 175, 114)"
                  , SvgA.points_
                      $ Hex.vertices Tall hexagon
                      # map (\(Point x y) -> show x <> "," <> show y)
                      # intercalate " "
                  ]
                  []
            , edgeId
                /\ \id -> Svg.polygon
                  [ SvgA.id_ id
                  , SvgA.strokeWidth_ $ show strokeWidth
                  , SvgA.stroke_ "black"
                  , SvgA.fill_ "black"
                  , SvgA.points_
                      $ Hex.vertices Tall hexagon
                      # map (\(Point x y) -> show x <> "," <> show y)
                      # intercalate " "
                  ]
                  []
            , connectedStoneId
                /\ \id -> Svg.g [ SvgA.id_ id ]
                  [ Svg.circle
                      [ SvgA.fill_ "black"
                      , SvgA.r_ $ show $ 0.9 * Hex.apo hexagon
                      ]
                      []
                  , Svg.circle
                      [ SvgA.fill_ "white"
                      , SvgA.r_ $ show $ 0.2 * 0.9 * Hex.apo hexagon
                      ]
                      []
                  ]
            , disconnectedStoneId
                /\ \id -> Svg.circle
                  [ SvgA.id_ id
                  , SvgA.fill_ "black"
                  , SvgA.r_ $ show $ 0.9 * Hex.apo hexagon
                  ]
                  []
            , enemyStoneId
                /\ \id -> Svg.g [ SvgA.id_ id ]
                  [ Svg.circle
                      [ SvgA.fill_ "white"
                      , SvgA.r_ $ show $ 0.975 * 0.9 * Hex.apo hexagon
                      ]
                      []
                  ]
            ] <#> \(i /\ f) -> f i
          )
      , content
      ]
  hexagon = Circ 1.0
  strokeWidth = 0.05

  hexSvgId :: String
  hexSvgId = "hexagon"

  edgeId :: String
  edgeId = "edge"

  connectedStoneId :: String
  connectedStoneId = "connected-stone"

  disconnectedStoneId :: String
  disconnectedStoneId = "stone"

  enemyStoneId :: String
  enemyStoneId = "enemy"

  placeHexagon :: IPoint -> Nut
  placeHexagon hexGridPos =
    let
      point = Hex.gridPoint hexagon hexGridPos
    in
      Svg.use
        [ SvgA.href_ $ "#" <> hexSvgId
        , SvgA.transform_
            $ "translate("
            <> show (Point.x point)
            <> ","
            <> show (Point.y point)
            <> ")"
        ]
        []

  placeEdge :: IPoint -> Nut
  placeEdge hexGridPos =
    let
      point = Hex.gridPoint hexagon hexGridPos
    in
      Svg.use
        [ SvgA.href_ $ "#" <> edgeId
        , SvgA.transform_
            $ "translate("
            <> show (Point.x point)
            <> ","
            <> show (Point.y point)
            <> ")"
        ]
        []

  placeStones :: Stone -> Nut
  placeStones { connected, pos } =
    let
      point = Hex.gridPoint hexagon pos
    in
      Svg.use
        [ SvgA.href_ $ "#" <>
            if connected then
              connectedStoneId
            else
              disconnectedStoneId
        , SvgA.transform_
            $ "translate("
            <> show (Point.x point)
            <> ","
            <> show (Point.y point)
            <> ")"
        ]
        []

  placeEnemyStones :: IPoint -> Nut
  placeEnemyStones hexGridPoint =
    let
      point = Hex.gridPoint hexagon hexGridPoint
    in
      Svg.use
        [ SvgA.href_ $ "#" <> enemyStoneId
        , SvgA.transform_
            $ "translate("
            <> show (Point.x point)
            <> ","
            <> show (Point.y point)
            <> ")"
        ]
        []

  makeViewBox :: NonEmptyArray IPoint -> String
  makeViewBox positions =
    let
      { min, max } = Point.box $ fold1 $ svgGridPoints <$> positions
      strokeOffset =
        Point
          (0.5 * strokeWidth)
          (0.5 * Hex.ratio * strokeWidth)
    in
      box2viewBox
        { min: min - strokeOffset
        , max: max + strokeOffset
        }
    where
    box2viewBox :: Box Number -> String
    box2viewBox { min, max } =
      let
        diff = max - min
        width = Point.x diff
        height = Point.y diff
      in
        [ Point.x min, Point.y min, width, height ]
          <#> show
          # intercalate " "

    svgGridPoints :: IPoint -> NonEmptyArray NPoint
    svgGridPoints pos = add (Hex.gridPoint hexagon pos) <$> Hex.vertices Tall hexagon

fill :: CarrierData -> NonEmptyArray IPoint
fill { cells, endpoints } =
  case isFinished of
    Just ones ->
      let
        start = minWith Point.x ones
        finish = maxWith Point.x ones
        bottomBorder = Point.x start .. Point.x finish <#> (Point ~$ 0)

        go :: Int -> Maybe IPoint
        go i =
          let
            next = start + Point i 0
          in
            if not elem next cells then
              Just next
            else if next /= finish then
              go $ i + 1
            else
              Nothing
      in
        case go 1 of
          Just ffStart -> floodFill ffStart (cells <> bottomBorder)
            # Ne.filter ((/=) 0 <. Point.y)
            # Ne.fromArray
            # case _ of
                Just a -> a
                Nothing -> unsafeThrow "something has gone very wrong"
          Nothing -> cells
    Nothing -> cells
  where
  isFinished :: Maybe (NonEmptyArray IPoint)
  isFinished = do
    let
      bottom = Ne.filter ((==) 1 <. Point.y) cells
      e1 /\ e2 = unEndpoints endpoints
    if elem e1 bottom && elem e2 bottom && e1 /= e2 then
      Ne.fromArray bottom
    else
      Nothing

  floodFill :: IPoint -> NonEmptyArray IPoint -> NonEmptyArray IPoint
  floodFill start filled =
    if elem start filled then
      filled
    else
      (filled <> Ne.singleton start)
        # continue C1
        # continue C3
        # continue C5
        # continue C7
        # continue C9
        # continue C11
    where
    continue :: Clock -> NonEmptyArray IPoint -> NonEmptyArray IPoint
    continue c =
      floodFill (dest $ applyMovement start $ clockMove c)

minWith :: ∀ f a b. Foldable1 f => Ord b => (a -> b) -> f a -> a
minWith f = foldl1 (\acc a -> if f a < f acc then a else acc)

maxWith :: ∀ f a b. Foldable1 f => Ord b => (a -> b) -> f a -> a
maxWith f = foldl1 (\acc a -> if f a > f acc then a else acc)

type TemplateSpec =
  { stones :: NonEmptyArray Stone
  , carrierMoves :: Array Move
  , enemyStones :: Array IPoint
  }

templateSpecParser :: Parser TemplateSpec
templateSpecParser = do
  { height, stoneMoves } <- stonesParser
  let start = Point 1 height
  case Stone.placeStones start stoneMoves of
    Right stones -> do
      oneOf
        [ Sp.try do
            _ <- Sp.char '-'
            carrierMoves <- Array.fromFoldable <$> movesParser
            enemyStones <- oneOf
              [ Sp.try do
                  _ <- Sp.char '-'
                  enemyMoves <- manyStoneMovesParser
                  case Stone.placeEnemyStones start enemyMoves of
                    Right estones -> pure estones
                    Left e -> Sp.fail $ show e

              , pure []
              ]
            pure
              { stones
              , carrierMoves
              , enemyStones
              }
        , pure { stones, carrierMoves: [], enemyStones: [] }
        ]
    Left e -> Sp.fail $ show e
