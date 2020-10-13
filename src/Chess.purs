module Chess where

import Effect (Effect)
import Control.Promise (Promise, toAffE)
import Effect.Aff (Aff)

foreign import fenIsValid :: String -> Boolean

-- Removes move numbers and unnecessary en passant info
-- Behavior is undefined when (fenIsValid fen) == false
foreign import sanitizeFEN :: String -> String

foreign import getMoveImpl :: String -> String -> Effect (Promise String)

-- Since the app only gets one move from the user after setting up the board each time
-- the set-up-board and return-move-string logic can be implemented together as an Aff
getMove :: String -> String -> Aff String
getMove fen move = toAffE (getMoveImpl fen move) -- Try to make this point-free later