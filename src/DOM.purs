module DOM where

import Prelude (Unit)
import Web.Event.Internal.Types (Event)
import Effect (Effect)
  
-- Some DOM-related helpers that are significantly more concise
-- via FFI than if implemented with existing PureScript libraries.

-- Gets the (key) code from event generated in particular 
-- by the "keydown" event listener.
foreign import eventCode :: Event -> String

-- Clicks the button with the given id.
-- Fails silently if the button can't be clicked or doesn't exist.
foreign import click :: String -> Effect Unit