module Chess where

import Prelude

import Effect (Effect)
import Control.Promise (Promise, toAffE)
import Effect.Aff (Aff)
import Types

foreign import fenIsValid :: String -> Boolean

-- Removes move numbers and unnecessary en passant info
-- Behavior is undefined when (fenIsValid fen) == false
foreign import sanitizeFEN :: String -> String

-- Since the app only gets one move from the user after setting up the board each time,
-- the set-up-board and return-move-string logic can be implemented together as an Aff.
-- The second parameter, the expected move, is only for board cosmetics. In quiz mode,
-- the board will flash green/red depending if the user move matches the expected move.
-- For "new puzzle" board cosmetics, give an empty expected move.
getMove :: FEN -> Move -> Aff Move
getMove fen expectedMove = toAffE $ getMoveImpl fen expectedMove

-- We need this "impl" function since it's easiest to 
-- import as Effect (Promise _) and then convert it to an Aff.
foreign import getMoveImpl :: String -> String -> Effect (Promise String)