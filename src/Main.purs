module Main where

import MasonPrelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, cons')
import Data.Array.NonEmpty as Ne
import Data.Bifunctor (lmap)
import Data.Foldable (elem)
import Data.Semigroup.Foldable (fold1)
import Deku.Core (Nut, fixed)
import Deku.DOM (text_)
import Deku.DOM as D
import Deku.DOM.Attributes as A
import Deku.DOM.Combinators (runOn_)
import Deku.DOM.Listeners as L
import Deku.DOM.SVG as Svg
import Deku.DOM.SVG.Attributes as SvgA
import Deku.Do as Deku
import Deku.Hooks (useState, useState', (<#~>))
import Deku.Toplevel (runInBody)
import Hexagon (Hexagon)
import Hexagon as Hex
import Magic (magic)
import Movement
  ( Clock(C11, C9, C7, C5, C3, C1)
  , applyMovement
  , clockMove
  , clockPath
  , dest
  , movesParser
  )
import Point (Box, IPoint, NPoint, Point(..))
import Point as Point
import StringParser (printParserError, runParser)

main :: Effect Unit
main = do
  void $ runInBody Deku.do
    let
      strokeWidth = 0.05
      hexagon = Hex.fromWidth 1.0
    set /\ poll <- magic
      { height: useState 3
      , stepsStr: useState "4.7."
      }
    let
      hexagonsP = lift2
        ( \height stepsStr -> do
            moves <- lmap printParserError (runParser movesParser stepsStr)
            points <- clockPath (Point 1 height) moves
            pure points
        )
        poll.height
        poll.stepsStr
    D.div [ A.style_ "height: 100vh;" ]
      [ D.input
          [ A.xtypeNumber
          , A.min_ "1"
          , A.step_ "1"
          , A.value $ show <$> poll.height
          , L.numberOn_ L.input $ round .> set.height
          ]
          []
      , D.input [ A.value poll.stepsStr, L.valueOn_ L.input set.stepsStr ] []
      , hexagonsP <#~> case _ of
          Right hexagons ->
            Svg.svg
              [ SvgA.width_ "100%"
              , SvgA.height_ "100%"
              , SvgA.viewBox_ $ makeViewBox hexagon strokeWidth hexagons
              , SvgA.transform_ "scale(1 -1)"
              -- , pure $ attributeAtYourOwnRisk "style"
              --     "position: absolute; bottom: 0; left: 0"
              ]
              [ Svg.defs_
                  [ Svg.polygon
                      [ SvgA.id_ "hexagon"
                      , SvgA.strokeWidth_ $ show strokeWidth
                      , SvgA.stroke_ "blue"
                      , SvgA.fill_ "red"
                      , SvgA.points_
                          $ svgPoints hexagon
                          # map (\(Point x y) -> show x <> "," <> show y)
                          # intercalate " "
                      ]
                      []
                  ]
              , fixed $ Array.fromFoldable
                    $ placeHexagon hexagon
                    <$> hexagons
              ]
          Left error -> text_ error
      ]

placeHexagon :: Hexagon ->  IPoint -> Nut
placeHexagon h hexGridPos =
  let
    point = Hex.gridPoint h hexGridPos
  in
    Svg.use
      [ SvgA.href_ "#hexagon"
      , SvgA.transform_
          $ "translate("
          <> show (Point.x $ point)
          <> " "
          <> show (Point.y point)
          <> ")"
      , runOn_ L.click $ logShow hexGridPos
      ]
      []

makeViewBox :: Hexagon -> Number -> NonEmptyArray IPoint -> String
makeViewBox h strokeWidth positions =
  let
    { min, max } = Point.box $ fold1 $ svgGridPoints h <$> positions
    strokeOffset = Point
      (0.5 * strokeWidth)
      (0.5 * Hex.height (Hex.fromWidth strokeWidth))
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

svgPoints :: Hexagon -> NonEmptyArray NPoint
svgPoints h = cons'
  (Point 0.0 (Hex.height h / 2.0))
  [ Point (Hex.width h / 2.0) (Hex.side h / 2.0)
  , Point (Hex.width h / 2.0) (-Hex.side h / 2.0)
  , Point 0.0 (-Hex.height h / 2.0)
  , Point (-Hex.width h / 2.0) (-Hex.side h / 2.0)
  , Point (-Hex.width h / 2.0) (Hex.side h / 2.0)
  ]

svgGridPoints :: Hexagon -> IPoint -> NonEmptyArray NPoint
svgGridPoints h pos = add (Hex.gridPoint h pos) <$> svgPoints h

floodFill :: IPoint -> NonEmptyArray IPoint -> NonEmptyArray IPoint
floodFill start filled =
  if elem start filled then
    filled
  else
    (Ne.singleton start <> filled)
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
