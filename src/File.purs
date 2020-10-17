module File where

import Prelude
import Effect (Effect)
  
-- First parameter is the name of the file (including the extension)
-- Second parameter is the contents of the file
foreign import saveFile :: String -> String -> Effect Unit