module Svg (makeSvg, AddIds, ToUse, Use) where

import Lude

import Data.Array ((:))
import Data.Array as Array
import Data.Symbol (class IsSymbol, reflectSymbol)
import Deku.Core (Attribute, Nut)
import Deku.DOM.SVG (SVGSVGElement, SVGUseElement)
import Deku.DOM.SVG as Svg
import Deku.DOM.SVG.Attributes as SvgA
import FRP.Poll (Poll)
import Heterogeneous.Folding
  ( class FoldingWithIndex
  , class HFoldlWithIndex
  , hfoldlWithIndex
  )
import Heterogeneous.Mapping
  ( class HMapWithIndex
  , class MappingWithIndex
  , hmapWithIndex
  )

makeSvg
  :: ∀ r defs
   . HFoldlWithIndex AddIds (Array Nut) r (Array Nut)
  => HMapWithIndex ToUse r defs
  => Array (Poll (Attribute (SVGSVGElement ())))
  -> r
  -> (defs -> Array Nut)
  -> Nut
makeSvg attrs defs defsToBody =
  Svg.svg attrs
    $ Array.cons
        (Svg.defs_ $ hfoldlWithIndex AddIds ([] :: Array Nut) defs)
        (defsToBody $ hmapWithIndex ToUse defs)

data ToUse = ToUse

type Use = Array (Poll (Attribute (SVGUseElement ()))) -> Nut

instance IsSymbol l => MappingWithIndex ToUse (Proxy l) a Use where
  mappingWithIndex _ id _ attrs =
    Svg.use (SvgA.href_ ("#" <> makeId  (reflectSymbol id)) : attrs) []

data AddIds = AddIds

instance
  IsSymbol l =>
  FoldingWithIndex AddIds (Proxy l) (Array Nut) Nut (Array Nut) where
  foldingWithIndex _ id acc def = Array.snoc acc $
    Svg.g
      [ SvgA.id_ $ makeId $ reflectSymbol id ]
      [ def ]

makeId :: String -> String
makeId s = s <> "_typesafe"
