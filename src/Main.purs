module Main where

import Lude

import Data.Array as Array
import Data.Array.NonEmpty ((..))
import Data.Array.NonEmpty as Ne
import Data.Bifunctor (lmap)
import Data.Foldable (elem)
import Data.Semigroup.Foldable (class Foldable1, fold1, foldl1)
import Deku.Core (Nut, fixed)
import Deku.DOM (text_)
import Deku.DOM as D
import Deku.DOM.Attributes as A
import Deku.DOM.Listeners as L
import Deku.DOM.SVG as Svg
import Deku.DOM.SVG.Attributes as SvgA
import Deku.DOM.Self (selfT_)
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Deku.Toplevel (runInBody)
import Effect.Timer (setTimeout)
import FRP.Poll (Poll)
import Hexagon (Hexagon(Circ), Orientation(Tall))
import Hexagon as Hex
import Magic (magic)
import Movement
  ( Clock(C11, C9, C7, C5, C4, C3, C1)
  , Endpoints
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
import Stone (Stone)
import Stone as Stone
import StringParser (printParserError, runParser)
import Web.HTML.HTMLElement (focus)
import Web.HTML.HTMLInputElement as HTMLInputElem

main :: Effect Unit
main = do
  void $ runInBody Deku.do
    set /\ poll <- magic
      { height: useState 3
      , stepsStr: useState "4.7."
      }
    let
      svgDataP :: Poll (Either String SvgData)
      svgDataP = lift2
        ( \height stepsStr ->
            let
              start = Point 1 height
            in
              if stepsStr == "" then do
                pure
                  { endpoints: mkEndpoints start start
                  , hexPoints: pure start
                  , stones:
                      cons'
                        (Stone.connected start)
                        [ Stone.disconnected $ Point 1 1 ]
                  }
              else do
                moves <- lmap printParserError (runParser movesParser stepsStr)
                points <- movesPath start moves
                endpoints <- case movements moves of
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
                stones <- Stone.placeStones start $
                  cons'
                    { reset: false
                    , connected: false
                    , moves: pure C4
                    }
                    [ { reset: true
                      , connected: false
                      , moves: cons' (C7) [ C7 ]
                      }
                    ]

                pure
                  { endpoints
                  , hexPoints:
                      fill
                        { endpoints
                        , hexPoints: Ne.nub points
                        , stones
                        }
                  , stones
                  }
        )
        poll.height
        poll.stepsStr
    D.div [ A.style_ "height: 100vh;" ]
      [ inputs set poll
      , hexagonSvgs svgDataP
      ]
  where
  inputs set poll =
    D.div [ A.style_ "position: absolute; z-index: 1" ]
      [ D.input
          [ A.xtypeNumber
          , A.min_ "1"
          , A.step_ "1"
          , A.value $ show <$> poll.height
          , L.numberOn_ L.input $ round .> set.height
          , selfT_
              $ HTMLInputElem.toHTMLElement
              .> focus
              .> setTimeout 0
              .> void
          ]
          []
      , D.input [ A.value poll.stepsStr, L.valueOn_ L.input set.stepsStr ] []
      ]

type SvgData =
  { endpoints :: Endpoints
  , hexPoints :: NonEmptyArray IPoint
  , stones :: NonEmptyArray Stone
  }

hexagonSvgs :: Poll (Either String SvgData) -> Nut
hexagonSvgs svgDataP =
  svgDataP <#~> case _ of
    Right { endpoints, hexPoints, stones } ->
      let
        edgePoints = edge endpoints
        allPoints = hexPoints <> edgePoints <> (stones <#> _.pos)
      in
        Svg.svg
          [ SvgA.width_ "100%"
          , SvgA.height_ "100%"
          , SvgA.viewBox_ $ makeViewBox allPoints
          , SvgA.transform_ "scale(1,-1)"
          ]
          [ Svg.defs_
              [ Svg.polygon
                  [ SvgA.id_ hexSvgId
                  , SvgA.strokeWidth_ $ show strokeWidth
                  , SvgA.stroke_ "blue"
                  , SvgA.fill_ "red"
                  , SvgA.points_
                      $ Hex.vertices Tall hexagon
                      # map (\(Point x y) -> show x <> "," <> show y)
                      # intercalate " "
                  ]
                  []
              , Svg.polygon
                  [ SvgA.id_ edgeId
                  , SvgA.strokeWidth_ $ show strokeWidth
                  , SvgA.stroke_ "white"
                  , SvgA.fill_ "black"
                  , SvgA.points_
                      $ Hex.vertices Tall hexagon
                      # map (\(Point x y) -> show x <> "," <> show y)
                      # intercalate " "
                  ]
                  []
              , Svg.g [ SvgA.id_ connectedStoneId ]
                  [ Svg.circle
                      [ SvgA.fill_ "black"
                      , SvgA.r_ $ show $ 0.90 * Hex.apo hexagon
                      ]
                      []
                  ]
              , Svg.circle
                  [ SvgA.id_ disconnectedStoneId
                  , SvgA.fill_ "white"
                  , SvgA.r_ $ show $ 0.90 * Hex.apo hexagon
                  ]
                  []
              ]
          , fixed $ Array.fromFoldable $ placeHexagon <$> hexPoints
          , fixed $ Array.fromFoldable $ placeEdge <$> edgePoints
          , fixed $ Array.fromFoldable $ placeStones <$> stones
          ]
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

fill :: SvgData -> NonEmptyArray IPoint
fill { endpoints, hexPoints } =
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
            if not elem next hexPoints then
              Just next
            else if next /= finish then
              go $ i + 1
            else
              Nothing
      in
        case go 1 of
          Just ffStart -> floodFill ffStart (hexPoints <> bottomBorder)
            # Ne.filter ((/=) 0 <. Point.y)
            # Ne.fromArray
            # case _ of
                Just a -> a
                Nothing -> unsafeThrow "something has gone very wrong"
          Nothing -> hexPoints
    Nothing -> hexPoints
  where
  isFinished :: Maybe (NonEmptyArray IPoint)
  isFinished = do
    let
      bottom = Ne.filter ((==) 1 <. Point.y) hexPoints
      e1 /\ e2 = unEndpoints endpoints
    if elem e1 bottom && elem e2 bottom then
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
