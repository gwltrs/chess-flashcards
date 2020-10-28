module Constants where

import Types (Days, TimestampSeconds)

firstBox :: Days
firstBox = 1

factorForNextBox :: Int
factorForNextBox = 2

maxBox :: Days
maxBox = 64

secondsInADay :: TimestampSeconds
secondsInADay = 86_400

maxVariance :: Number
maxVariance = 0.1