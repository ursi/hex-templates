module Magic (magic, Builder', Traverse, Split) where

import MasonPrelude

import Data.Bifunctor (bimap)
import Data.Symbol (class IsSymbol)
import Deku.Core (Hook)
import Deku.Do as Deku
import Heterogeneous.Folding
  ( class FoldingWithIndex
  , class HFoldlWithIndex
  , hfoldlWithIndex
  )
import Prim.Row (class Cons, class Lacks)
import Record.Builder (Builder, buildFromScratch, insert)

data Traverse = Traverse

instance
  ( IsSymbol l
  , Lacks l r1
  , Cons l a r1 r2
  ) =>
  FoldingWithIndex
    Traverse
    (Proxy l)
    (Hook (Builder r (Record r1)))
    (Hook a)
    (Hook (Builder r (Record r2))) where
  foldingWithIndex _ label hr1 ha = \f -> Deku.do
    a <- ha
    r1 <- hr1
    f $ r1 .> insert label a

data Split = Split

instance
  ( IsSymbol l
  , Lacks l r1
  , Lacks l r2
  , Cons l a r1 r3
  , Cons l b r2 r4
  ) =>
  FoldingWithIndex
    Split
    (Proxy l)
    (Builder r (Record r1) /\ Builder r (Record r2))
    (a /\ b)
    (Builder r (Record r3) /\ Builder r (Record r4)) where
  foldingWithIndex _ label (r1 /\ r2) (a /\ b) =
    r1 .> insert label a /\ r2 .> insert label b

type Builder' a = Builder {} (Record a)

magic
  :: ∀ a b c d
   . HFoldlWithIndex Traverse
       (Hook (Builder' ()))
       a
       (Hook (Builder' b))
  => HFoldlWithIndex
       Split
       (Builder' () /\ Builder' ())
       (Record b)
       (Builder' c /\ Builder' d)
  => a
  -> Hook (Record c /\ Record d)
magic declarations = \f ->
  hfoldlWithIndex Traverse
    ((#) identity :: Hook (Builder {} {}))
    declarations
    ( buildFromScratch
        .> hfoldlWithIndex Split
          ((identity :: Builder {} {}) /\ (identity :: Builder {} {}))
        .> bimap buildFromScratch buildFromScratch
        .> f
    )

