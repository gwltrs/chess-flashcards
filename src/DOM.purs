module DOM where

import Prelude (Unit)
import Web.Event.Internal.Types (Event)
import Effect (Effect)
  
foreign import eventCode :: Event -> String

foreign import click :: String -> Effect Unit