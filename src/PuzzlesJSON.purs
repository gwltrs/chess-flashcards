module PuzzlesJSON where

import Data.Maybe (Maybe(..))
import Simple.JSON (readJSON, writeJSON)

import Types (Puzzle)

-- Wrapper interfaces for JSON parsing/serialization.
-- Even though tested lib(s) will likely be imported,
-- defining these first are necessary for implementing
-- roundtrip tests in a TDD sequence (writing tests first).

makePuzzlesJSON :: Array Puzzle -> String
makePuzzlesJSON puzzles = ""

parsePuzzlesJSON :: String -> Maybe (Array Puzzle)
parsePuzzlesJSON jsonString = Nothing