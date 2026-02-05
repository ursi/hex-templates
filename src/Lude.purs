module Lude
  ( module Exports
  , unsafeFromRight
  , unsafeParseInt
  , logInfo
  ) where

import MasonPrelude

import Control.Monad.Error.Class (class MonadError, throwError) as Exports
import Data.Array.NonEmpty (NonEmptyArray, cons') as Exports
import Data.Newtype (class Newtype) as Exports
import MasonPrelude as Exports
import Partial.Unsafe (unsafePartial)
import Partial.Unsafe (unsafePartial) as Exports

foreign import unsafeParseInt :: String -> Int
foreign import logInfo :: ∀ a b. a -> b -> b

unsafeFromRight :: ∀ l r. Either l r -> r
unsafeFromRight e = unsafePartial case e of
  Right r -> r
