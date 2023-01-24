module Utils where

import Prelude

import Data.Either (Either(..))
import Partial.Unsafe (unsafeCrashWith)

unsafeRight :: forall a b. Either a b -> b
unsafeRight (Right r) = r
unsafeRight _ = unsafeCrashWith "unsafeRight: expected Right but found Left"