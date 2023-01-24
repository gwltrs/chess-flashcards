module PuzzlesJSON where

import Data.Semigroup ((<>))
import Data.Maybe (Maybe(..))
import Simple.JSON (readJSON)
import Data.Either (Either(..))
import Data.Functor ((<#>))
import Data.Show
import Data.String.Common (joinWith)
import Data.Function ((#))

import Types (Puzzle)

-- Wrapper interfaces for JSON parsing/serialization.
-- Even though tested lib(s) will likely be imported,
-- defining these are for implementing roundtrip tests.

makePuzzlesJSON :: Array Puzzle -> String
makePuzzlesJSON = makePuzzlesJSONImpl

-- Now implementing this manually since the prebuilt solutions
-- aren't generating the puzzle JSON properties in a deterministic ordering. 
makePuzzlesJSONImpl :: Array Puzzle -> String
makePuzzlesJSONImpl puzzles = puzzles
  <#> (\p -> 
    "  {\n" <>
    "    \"name\": \"" <> p.name <> "\",\n" <>
    "    \"fen\": \"" <> p.fen <> "\",\n" <>
    "    \"move\": \"" <> p.move <> "\",\n" <>
    "    \"box\": " <> show p.box <> ",\n" <>
    "    \"lastDrilledAt\": " <> show p.lastDrilledAt <> "\n" <>
    "  }")
  # joinWith ",\n"
  # (\ps -> "[\n" <> ps <> "\n]")

parsePuzzlesJSON :: String -> Maybe (Array Puzzle)
parsePuzzlesJSON jsonString =
  case readJSON jsonString of
    Right (r :: Array Puzzle) -> 
      Just r
    Left _ -> 
      Nothing