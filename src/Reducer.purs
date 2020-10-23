module Reducer where

import Prelude ((-), (>), compare)
import Data.Maybe (Maybe(..), isJust)
import Data.HeytingAlgebra ((||), not)
import Data.String (trim, length)
import Data.Eq ((==))
import Data.Array (elemIndex, filter, mapMaybe, sortBy, null)
import Data.Array.NonEmpty (toArray)
import Data.Function ((#))
import Data.Functor (map)
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (RegexFlags(..))
import Data.Either (fromRight)
import Partial.Unsafe (unsafePartial)
import Data.Int (fromString)
import Data.Foldable (foldr)
import Data.Ord (max)
import Data.Show (show)
import Data.Semigroup ((<>))
import Data.Semiring (add, (*))
import Data.String.Common (localeCompare)

import Types (Action(..), Alert(..), Puzzle, State, View(..), Name, TimestampSeconds)
import Constants (firstBox)
import Chess (fenIsValid, sanitizeFEN)
import PuzzlesJSON (parsePuzzlesJSON)

reducer :: State -> Action -> State
reducer state action =
  case { act: action, vw: state.view } of
    { act: NewFile, vw: _ } ->
      state { view = MainMenu "" "" }
    { act: UpdatePuzzleName newName, vw: MainMenu _ currentFEN } ->
      state { view = MainMenu newName currentFEN }
    { act: UpdateFEN newFEN, vw: MainMenu currentName _ } ->
      state { view = MainMenu currentName newFEN }
    { act: CreatePuzzle, vw: MainMenu name fen } ->
      let
        trimmedName = trim name
        trimmedFEN = trim fen
        sanitizedFEN = sanitizeFEN trimmedFEN
      in
        if (length trimmedName) == 0 || (length trimmedFEN) == 0 then
          state { alert = Just MissingNameOrFEN }
        else if not (fenIsValid fen) then 
          state { alert = Just InvalidFEN }
        else if state.puzzles # (map \p -> p.name) # elemIndex trimmedName # isJust then
          state { alert = Just DuplicateName }
        else if state.puzzles # (map \p -> p.fen) # elemIndex sanitizedFEN # isJust then
          state { alert = Just DuplicateFEN }
        else
          state { view = CreatingPuzzle (incrementName state.puzzles trimmedName) sanitizedFEN Nothing }
    { act: BackToMain, vw: _ } ->
      state { view = MainMenu "" "" }
    { act: AddMoveToNewPuzzle move, vw: CreatingPuzzle name fen _ } ->
      state { view = CreatingPuzzle name fen (Just move) }
    { act: SavePuzzle, vw: CreatingPuzzle name fen (Just move) } ->
      let
        newPuzzle = {
          name: name,
          fen: fen,
          move: move,
          box: firstBox,
          lastDrilledAt: 0
        }
        comparePuzzles l r =
          localeCompare l.name r.name
      in
        state { puzzles = sortBy comparePuzzles (state.puzzles <> [newPuzzle]), view = MainMenu "" "" }
    { act: LoadFile fileString currentTimestamp, vw: LoadingFile } ->
      case parsePuzzlesJSON fileString of
        Just puzzles ->
          state { 
            puzzles = puzzles, 
            reviewStack = (generateReviewStack currentTimestamp puzzles),
            view = MainMenu "" "" }
        Nothing -> 
          state { alert = Just InvalidFile }
    { act: Review, vw: MainMenu _ _ } ->
      if null state.reviewStack then
        state { alert = Just NoDrillsForReview }
      else
        state
    { act: _, vw: _ } ->
      state

-- Assumes the name is already trimmed at this point
incrementName :: Array Puzzle -> String -> String
incrementName puzzles name = 
  let 
    flags = RegexFlags { global: false, ignoreCase: false, multiline: true, sticky: false, unicode: false }
    nameRegex = unsafePartial (fromRight (regex "^(\\S.*\\S)\\s+\\?$" flags))
    nameMatch = match nameRegex name
    puzzlesRegex = unsafePartial (fromRight (regex "^(\\S.*\\S)\\s+([1-9][0-9]*)$" flags))
    puzzleMatches = 
      puzzles 
        # (map \p -> p.name) 
        # (mapMaybe (match puzzlesRegex))
        # (mapMaybe \nonEmptyArr -> 
            case toArray nonEmptyArr of
              [Just _, Just nameString, Just intString] ->
                fromString intString # (map \parsedInt -> { name: nameString, int: parsedInt })
              _ -> 
                Nothing
          )
  in
    case nameMatch # map toArray of
      Just [Just _, Just nameWithQuestionMark] -> 
        nameWithQuestionMark <> " " <> (currentIncrement # add 1 # show)
          where
            currentIncrement = puzzleMatches
              # (filter \match -> match.name == nameWithQuestionMark)
              # (map \m -> m.int)
              # (foldr max 0)
      _ -> 
        name

generateReviewStack :: TimestampSeconds -> Array Puzzle -> Array Name
generateReviewStack nowSeconds puzzles =
  let
    secondsInADay = 60 * 60 * 24
  in
    puzzles
      # (map \p -> { name: p.name, overdue: nowSeconds - p.lastDrilledAt - (secondsInADay * p.box) })
      # (filter \tuple -> tuple.overdue > 0)
      # (sortBy \l r -> compare r.overdue l.overdue)
      # (map \tuple2 -> tuple2.name)