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
import Deku.DOM.Self (selfT_)
import Deku.Do as Deku
import Deku.Hooks (useState, useState', (<#~>))
import Deku.Toplevel (runInBody)
import Effect.Timer (setTimeout)
import Hexagon (Hexagon(Circ), Orientation(Tall))
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
import Web.HTML.HTMLElement (focus)
import Web.HTML.HTMLInputElement as HTMLInputElem

hexSvgId :: String
hexSvgId = "hexagon"

main :: Effect Unit
main = do
  void $ runInBody Deku.do
    let
      strokeWidth = 0.05
      hexagon = Circ 1.0
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
      [ D.div [ A.style_ "position: absolute" ]
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
      , hexagonsP <#~> case _ of
          Right hexagons ->
            Svg.svg
              [ SvgA.width_ "100%"
              , SvgA.height_ "100%"
              , SvgA.viewBox_ $ makeViewBox hexagon strokeWidth hexagons
              , SvgA.transform_ "scale(1 -1)"
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
                  ]
              , fixed $ Array.fromFoldable
                  $ placeHexagon hexagon
                  <$> hexagons
              ]
          Left error -> text_ error
      ]

placeHexagon :: Hexagon -> IPoint -> Nut
placeHexagon h hexGridPos =
  let
    point = Hex.gridPoint h hexGridPos
  in
    Svg.use
      [ SvgA.href_ $ "#" <> hexSvgId
      , SvgA.transform_
          $ "translate("
          <> show (Point.x point)
          <> " "
          <> show (Point.y point)
          <> ")"
      ]
      []

makeViewBox :: Hexagon -> Number -> NonEmptyArray IPoint -> String
makeViewBox h strokeWidth positions =
  let
    { min, max } = Point.box $ fold1 $ svgGridPoints h <$> positions
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

svgGridPoints :: Hexagon -> IPoint -> NonEmptyArray NPoint
svgGridPoints h pos = add (Hex.gridPoint h pos) <$> Hex.vertices Tall h

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
