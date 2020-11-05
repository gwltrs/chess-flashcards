module Clipboard where

import Prelude
import Effect (Effect)

-- Implementing own clipboard effect since other existing solutions
-- require existing DOM elements to use. This effect still uses the DOM
-- but does so under the hood: it creates and cleans up its own HTML.
foreign import copyToClipboard :: String -> Effect Unit