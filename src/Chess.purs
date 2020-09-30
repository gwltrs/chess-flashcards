module Chess where
  
foreign import fenIsValid :: String -> Boolean

-- Removes move numbers and unnecessary en passant info
-- Behavior is undefined when (fenIsValid fen) == false
foreign import sanitizeFEN :: String -> String