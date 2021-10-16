module Constants where

import Types (Days, TimestampSeconds)

secondsInADay :: TimestampSeconds
secondsInADay = 86_400

-- Used by space-key-to-next-button hotkey logic
nextButtonID :: String
nextButtonID = "nextButton"

-- The below constants (firstBox, factorForNextBox, maxBox, and maxVariance)
-- are defined to make it easier to tweak the spaced repetition logic.

firstBox :: Days
firstBox = 1

-- If puzzle is answered correctly in review mode: newBox = factorForNextBox * oldBox
factorForNextBox :: Int
factorForNextBox = 2

maxBox :: Days
maxBox = 192

-- See comments for the VarianceFactor type definition 
maxVariance :: Number
maxVariance = 0.1