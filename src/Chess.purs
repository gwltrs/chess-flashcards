module Chess where

import Prelude
import Effect (Effect)
import Control.Promise (Promise, toAffE)
import Effect.Aff (Aff)

foreign import fenIsValid :: String -> Boolean

-- Removes move numbers and unnecessary en passant info
-- Behavior is undefined when (fenIsValid fen) == false
foreign import sanitizeFEN :: String -> String

foreign import getMoveImpl :: String -> Effect (Promise String)

getMove :: String -> Aff String
getMove = getMoveImpl >>> toAffE