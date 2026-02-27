module Main where

import Lude

import Data.Array as Array
import Data.Array.NonEmpty ((..))
import Data.Array.NonEmpty as Ne
import Data.Bifunctor (lmap)
import Data.Foldable (elem, oneOf)
import Data.Semigroup.Foldable (class Foldable1, fold1, foldl1)
import Deku.Core (Attribute, Nut, fixed)
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
import Endpoints (Endpoints, mkEndpoints, unEndpoints)
import Endpoints as Endpoints
import FRP.Poll (Poll)
import Hexagon (Hexagon(Circ), Orientation(Tall))
import Hexagon as Hex
import Magic (magic)
import Movement
  ( Clock(C11, C9, C7, C5, C3, C1)
  , Move
  , applyMovement
  , clockMove
  , dest
  , movements
  , movesDest
  , movesParser
  , movesPath
  )
import Movement as Movement
import Point (Box, IPoint, NPoint, Point(..))
import Point as Point
import Stone (Stone, StoneMoves, manyStoneMovesParser, stonesParser)
import Stone as Stone
import StringParser (ParseError, Parser, runParser)
import StringParser as Sp
import Svg (Use, makeSvg)
import Syntax as Sntx
import Web.HTML.HTMLElement (focus)
import Web.HTML.HTMLInputElement as Input

main :: Effect Unit
main = do
  void $ runInBody Deku.do
    set /\ poll <- magic
      { specStr: useState "7:78^:7-4434.7787.-5:9:77"
      , showInstructions: useState false
      }
    let
      svgDataP :: Poll (Either Error SvgData)
      svgDataP = poll.specStr <#> \specStr ->
        if specStr == "" then do
          pure
            { stones: pure $ Stone.connected $ Point 1 1
            , mcarrier: Nothing
            , enemyStones: []
            }
        else do
          { stones, carrierMoves, enemyStones } <- parseTemplateSpec specStr
          case Ne.fromArray carrierMoves of
            Just carrierMovesNe -> do
              let start = (Ne.head stones).pos
              carrier <- lmap MovementError $ movesPath start carrierMovesNe
              endpoints <- case movements carrierMovesNe of
                [ m ] -> lmap MovementError do
                  endpoint <- movesDest start m
                  pure $ mkEndpoints start endpoint
                [ m1, m2 ] -> lmap MovementError do
                  endpoint1 <- movesDest start m1
                  endpoint2 <- movesDest start m2
                  pure $ mkEndpoints endpoint1 endpoint2
                ms -> throwError $ MovementsError (Array.length ms)
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
          align-items: center;
          """
      ]
      [ instructions set poll
      , inputs set poll
      , hexagonSvgs svgDataP
      ]
  where
  -- in a `where` so I don't thave to give it a type annotation
  inputs set poll =
    D.div
      [ A.style_
          """
          position: absolute;
          top: 10px;
          z-index: 1;
          """
      ]
      [ D.input
          [ A.style_
              """
              font-size: min(2rem, 4vh);
              border-radius: .25em;
              border: none;
              padding: .25em;
              min-width: 26ch;
              field-sizing: content;
              text-align: center;
              outline: none;
              """
          , A.value poll.specStr
          , L.valueOn_ L.input set.specStr
          , A.spellcheck_ "false"
          , selfT_ \i -> void $ setTimeout 0 $ focus $ Input.toHTMLElement i
          ]
          []
      ]

  instructions set poll =
    poll.showInstructions <#~> \showInstructions ->
      fixed
        [ D.button
            [ L.runOn_ L.click $ set.showInstructions $ not showInstructions
            , A.style_
                """
                position: absolute;
                top: 1em;
                left: 1em;
                z-index: 1;
                """
            ]
            [ text_ "instructions" ]
        , if showInstructions then
            let
              literal text = D.span [ A.klass_ "c0" ] [ text_ text ]
            in
              D.div
                [ A.style_
                    """
                    position: absolute;
                    top: 0;
                    left: 0;
                    display: flex;
                    justify-content: center;
                    align-items: center;
                    width: 100vw;
                    height: 100vh;
                    """
                ]
                [ D.div
                    [ A.style_
                        """
                        background: gray;
                        z-index: 1;
                        white-space: pre-wrap;
                        """
                    ]
                    [ text_ "The central tool is the "
                    , D.i__ "clock move"
                    , text_ ". for each of the numbers on the clock, there is a natural corresponding step one can take from a reference hexagon. Even numbers are bridges, and odd numbers are adjacent."
                    , D.code__
                        """

   c
a b 1 2
 9 - 3
8 7 5 4
   6

"""
                    , text_ "We use these, plus some shorthand to describe all the parts of a template:"
                    , D.h2__ "Stones"
                    , text_ "The stones section starts with a number that indicates the height of the anchor point. If it is a one stone template, that's all you need, otherwise a "
                    , literal ":"
                    , text_ " is placed after the anchor, and the other stones are specified as a colon separated list of clock move sequences to get from where you are, to the next stone. To place a stone relative to the anchor instead of the last placed stone, begin the sequence with "
                    , literal "*"
                    , text_ ". To denote a stone as being connected to opposite edge, end the sequence with "
                    , literal "^"
                    , text_ "."
                    , D.h2__ "Carrier"
                    , text_ "Carriers are specified with two clock move sequences, both starting from the anchor and ending at the edge. The two sequences describe the border of the carrier. Clock moves that correspond to bridges bring the whole bridge into the carrier. There are also three shorthand symbols:"
                    , D.ol
                        [ A.style_ "margin: 0;" ]
                        [ D.li []
                            [ literal "."
                            , text_ ": Continue this move until it forms a bridge to the edge."
                            ]
                        , D.li []
                            [ literal "z"
                            , text_ ": Continue this move until it \"drops to a ziggurat\". This is a common enough pattern that it gets a shorthand."
                            ]
                        , D.li []
                            [ literal "s"
                            , text_ ": Mirror the sequence."
                            ]
                        ]
                    , literal "."
                    , text_ " and "
                    , literal "z"
                    , text_ " indicate the end of a sequence, so the next move starts back at the anchor. In the case where the first sequence ends without one of those markers, "
                    , literal "*"
                    , text_ " is again used to set it back to the anchor (as I write this I see now that that's not strictly necessary, but I'll probably keep it in for readability)."
                    , D.h2__ "Enemy Stones/Holes"
                    , text_ "Enemy stones are specified the same way as stones, minus the initial anchor part and the "
                    , literal "^"
                    , text_ " syntax."
                    , D.h2__ "Sections"
                    , text_ "The sections are separated by "
                    , literal "-"
                    , text_ "s."
                    ]
                ]
          else
            mempty
        ]

type CarrierData =
  { cells :: NonEmptyArray IPoint
  , endpoints :: Endpoints
  }

type SvgData =
  { stones :: NonEmptyArray Stone
  , mcarrier :: Maybe CarrierData
  , enemyStones :: Array IPoint
  }

data Error
  = MovementError Movement.Error
  | MovementsError Int
  | ParserError ParseError

hexagonSvgs :: Poll (Either Error SvgData) -> Nut
hexagonSvgs svgDataP =
  svgDataP <#~> case _ of
    Right { mcarrier, stones, enemyStones } ->
      case mcarrier of
        Just carrier ->
          svgContent
            ( carrier.cells
                <> Endpoints.extendToEdge carrier.endpoints
                <> (stones <#> _.pos)
            )
            ( \use ->
                let
                  t u gridPoint = u [ translate hexagon gridPoint ]
                in
                  [ -- carrier svgs
                    t use.emptyCell <$> carrier.cells # Array.fromFoldable # fixed
                  , -- edge cells
                    t use.edgeCell <$> Endpoints.edge carrier.endpoints
                      # Array.fromFoldable
                      # fixed
                  , -- stones
                    placeStone use <$> stones # Array.fromFoldable # fixed
                  , -- enemy stones
                    t use.enemyStone <$> enemyStones # fixed
                  ]
            )
        Nothing ->
          svgContent
            (stones <#> _.pos)
            (\use -> Array.fromFoldable $ placeStone use <$> stones)
    Left e -> handleError e

  where
  svgContent
    :: NonEmptyArray IPoint
    -> ( { emptyCell :: Use
         , edgeCell :: Use
         , connectedStone :: Use
         , disconnectedStone :: Use
         , enemyStone :: Use
         }
         -> Array Nut
       )
    -> Nut
  svgContent allPoints content =
    D.div [ A.style_ "height: 70%; padding: 0 5%;" ]
      [ makeSvg
          [ SvgA.width_ "100%"
          , SvgA.height_ "100%"
          , SvgA.viewBox_ $ makeViewBox hexagon strokeWidth allPoints
          , SvgA.transform_ "scale(1,-1)"
          , SvgA.preserveAspectRatio_ "xMidYMin"
          ]
          { emptyCell:
              Svg.polygon
                [ SvgA.strokeWidth_ $ show strokeWidth
                , SvgA.stroke_ "black"
                , SvgA.points_
                    $ Hex.vertices Tall hexagon
                    # map (\(Point x y) -> show x <> "," <> show y)
                    # intercalate " "
                , SvgA.klass_ "c1"
                ]
                []
          , edgeCell:
              Svg.polygon
                [ SvgA.strokeWidth_ $ show strokeWidth
                , SvgA.stroke_ "black"
                , SvgA.fill_ "black"
                , SvgA.points_
                    $ Hex.vertices Tall hexagon
                    # map (\(Point x y) -> show x <> "," <> show y)
                    # intercalate " "
                ]
                []
          , connectedStone:
              Svg.g_
                [ Svg.circle
                    [ SvgA.fill_ "black"
                    , SvgA.r_ $ show stoneRadius
                    ]
                    []
                , Svg.circle
                    [ SvgA.fill_ "white"
                    , SvgA.r_ $ show $ 0.2 * stoneRadius
                    ]
                    []
                ]
          , disconnectedStone:
              Svg.circle
                [ SvgA.fill_ "black"
                , SvgA.r_ $ show stoneRadius
                ]
                []
          , enemyStone:
              Svg.g_
                [ Svg.circle
                    [ SvgA.fill_ "white"
                    -- white stones look larger if they are the same size
                    , SvgA.r_ $ show $ 0.975 * stoneRadius
                    ]
                    []
                ]
          }
          content
      ]

  handleError :: Error -> Nut
  handleError error =
    let
      highlight :: String -> Nut
      highlight text =
        D.span [ A.style_ "color: #bf0000; font-weight: bold;" ] [ text_ text ]

      highlightMove :: Move -> Nut
      highlightMove = Movement.unparse .> highlight
    in
      D.div
        [ A.style_
            """
              height: 100%;
              display: flex;
              justify-content: center;
              align-items: center;
              font-size: 3em;
              width: min(80%, 1100px);
              """
        ]
        [ D.span_ case error of
            MovementError me -> case me of
              Movement.BelowEdge clock ->
                [ text_ "The move "
                , highlightMove $ Movement.Step clock
                , text_ " takes you below the edge!"
                ]
              Movement.InvalidContinuation m ->
                let
                  symbol /\ meaning = case m of
                    Movement.ToEdge _ -> fromChar Sntx.toEdge /\ "\"continue this move untill you hit an edge\""
                    Movement.ToZiggurat _ -> fromChar Sntx.toZig /\ "\"continue until I can take an adjacent step downwards into a ziggurat\""
                    _ -> unsafeThrow "something has gone horribly wrong"
                in
                  [ highlightMove m
                  , text_ " is an invalid move. The "
                  , highlight symbol
                  , text_ $ " means " <> meaning <> ", but "
                  , highlightMove m
                  , text_ " will never hit an edge!"
                  ]
              Movement.TooLowForZiggurat m height ->
                [ text_ "You're trying to use "
                , highlightMove m
                , text_ $ " on row " <> show height <> ", but "
                , highlight $ fromChar Sntx.toZig
                , text_ " only works on rows 4 and up."
                ]
            MovementsError n -> [ text_ $ "The carrier needs to be specified with 2 paths from the start to the edge, but you currently have " <> show n <> "!" ]
            ParserError e ->
              [ text_ $ logInfo e "Your template encoding is not valid." ]
        ]

  hexagon = Circ 1.0
  strokeWidth = 0.075
  stoneRadius = 0.9 * Hex.apo hexagon

  placeStone
    :: ∀ r
     . { connectedStone :: Use, disconnectedStone :: Use | r }
    -> Stone
    -> Nut
  placeStone use stone =
    ( if stone.connected then
        use.connectedStone
      else
        use.disconnectedStone
    )
      [ translate hexagon stone.pos ]

translate
  :: ∀ f r
   . Applicative f
  => Hexagon
  -> IPoint
  -> f (Attribute (transform :: String | r))
translate hexagon gridPoint =
  let
    point = Hex.gridPoint hexagon gridPoint
  in
    SvgA.transform_
      $ "translate("
      <> show (Point.x point)
      <> ","
      <> show (Point.y point)
      <> ")"

makeViewBox :: Hexagon -> Number -> NonEmptyArray IPoint -> String
makeViewBox hexagon strokeWidth positions =
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

parseTemplateSpec :: String -> Either Error TemplateSpec
parseTemplateSpec input = do
  { height, stoneMoves, carrierMoves, enemyMoves } <-
    lmap ParserError $ runParser parser input
  let start = Point 1 height
  stones <- lmap MovementError $ Stone.placeStones start stoneMoves
  enemyStones <- lmap MovementError $ Stone.placeEnemyStones start enemyMoves
  pure { stones, carrierMoves, enemyStones }
  where
  parser
    :: Parser
         { height :: Int
         , stoneMoves :: Array StoneMoves
         , carrierMoves :: Array Move
         , enemyMoves :: Array StoneMoves
         }
  parser = do
    { height, stoneMoves } <- stonesParser
    oneOf
      [ allSoFar { height, stoneMoves, carrierMoves: [], enemyMoves: [] }
      , do
          _ <- Sp.char Sntx.sectionSep
          carrierMoves <- Array.fromFoldable <$> movesParser
          enemyMoves <- oneOf
            [ allSoFar []
            , do
                _ <- Sp.char Sntx.sectionSep
                enemyMoves <- manyStoneMovesParser
                Sp.eof
                pure enemyMoves
            ]
          pure
            { height
            , stoneMoves
            , carrierMoves
            , enemyMoves
            }
      ]

  allSoFar :: ∀ a. a -> Parser a
  allSoFar = Sp.try <. ($>) (Sp.eof <|> (Sp.char Sntx.sectionSep *> Sp.eof))
