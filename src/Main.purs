module Main where

import MasonPrelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, cons', (..))
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
    set /\ poll <- magic
      { height: useState 3
      , stepsStr: useState "4.7."
      }
    let
      hexagonsP = lift2
        ( \height stepsStr -> do
            moves <- lmap printParserError (runParser movesParser stepsStr)
            points <- clockPath (Point 1 height) moves
            pure $ fill $ Ne.nub points
        )
        poll.height
        poll.stepsStr
    D.div [ A.style_ "height: 100vh;" ]
      [ inputs set poll
      , hexagonSvgs hexagonsP
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

hexagonSvgs :: Poll (Either String (NonEmptyArray IPoint)) -> Nut
hexagonSvgs hexagonsP =
  hexagonsP <#~> case _ of
    Right hexagons ->
      Svg.svg
        [ SvgA.width_ "100%"
        , SvgA.height_ "100%"
        , SvgA.viewBox_ $ makeViewBox hexagons
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
            ]
        , fixed $ Array.fromFoldable $ placeHexagon <$> hexagons
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

fill :: NonEmptyArray IPoint -> NonEmptyArray IPoint
fill hexagons =
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
            if not elem next hexagons then
              Just next
            else if next /= finish then
              go $ i + 1
            else
              Nothing
      in
        case go 1 of
          Just ffStart -> floodFill ffStart (hexagons <> bottomBorder)
            # Ne.filter ((/=) 0 <. Point.y)
            # Ne.fromArray
            # case _ of
                Just a -> a
                Nothing -> unsafeThrow "something has gone very wrong"
          Nothing -> hexagons
    Nothing -> hexagons
  where
  -- the template touches the ground at at least 2 points
  isFinished :: Maybe (NonEmptyArray IPoint)
  isFinished = do
    { head, tail } <- Array.uncons $ Ne.filter ((==) 1 <. Point.y) hexagons
    if tail /= [] then
      pure $ cons' head tail
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

-- works but is exponential because bridges introduce 2 equivalent paths
-- I could:
-- trim paths that are euqal except for a bridge
--   I think I just need to check if they only every have one difference in a row
--   and if they do, I can trim one of them
--     the problem is, it takes two steps to figure that out, and my algorithm only sees
--     one step apart
--       perhaps I need to switch to keeping the whole list of paths in each step,
--       and expanding it. that way I can trim as I go
-- change the bridge to only do the outside (would't work for 6s)
-- change the moves part to be two distinct parts. this seems like it fixes a lot of my problems
_bottom :: NonEmptyArray IPoint -> NonEmptyArray IPoint
_bottom hexagons =
  let
    side1 = minWith Point.y hexagons
    side2 = getFarthest side1
  in
    cons' side1 (pure side2)
  where
  getFarthest :: IPoint -> IPoint
  getFarthest start = Ne.last $ go (pure start)
    where
    go :: NonEmptyArray IPoint -> NonEmptyArray IPoint
    go path =
      ( cons'
          (step C1)
          [ step C3
          , step C5
          , step C7
          , step C9
          , step C11
          ]
      )
        # maxWith Ne.length
      where
      step :: Clock -> NonEmptyArray IPoint
      step c =
        let
          nextStep = dest $ applyMovement (Ne.last path) $ clockMove c
        in
          if elem nextStep hexagons && not elem nextStep path then
            go $ Ne.snoc path nextStep
          else
            path

minWith :: ∀ f a b. Foldable1 f => Ord b => (a -> b) -> f a -> a
minWith f = foldl1 (\acc a -> if f a < f acc then a else acc)

maxWith :: ∀ f a b. Foldable1 f => Ord b => (a -> b) -> f a -> a
maxWith f = foldl1 (\acc a -> if f a > f acc then a else acc)

