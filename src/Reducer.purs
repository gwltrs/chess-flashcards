module Reducer where

import Prelude ((-), (>), compare, (>>=), (*))
import Control.Apply (lift2)
import Data.Maybe (Maybe(..), isJust, fromMaybe, isNothing)
import Data.HeytingAlgebra ((||), not)
import Data.String (trim, length)
import Data.Eq ((==))
import Data.Array (elemIndex, filter, mapMaybe, sortBy, head, tail, findIndex, (!!), alterAt)
import Data.Array.NonEmpty (toArray)
import Data.Function ((#))
import Data.Functor (map)
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (multiline)
import Data.Either (fromRight)
import Partial.Unsafe (unsafePartial)
import Data.Int (fromString, toNumber, round)
import Data.Foldable (foldr, find)
import Data.Ord (max, min)
import Data.Show (show)
import Data.Semigroup ((<>))
import Data.Semiring (add, mul)
import Data.String.Common (localeCompare)
import Data.Tuple (Tuple(..), fst, snd)

import Types (Action(..), Alert(..), Puzzle, State, View(..), Name, TimestampSeconds, VarianceFactor)
import Constants (firstBox, factorForNextBox, maxBox, secondsInADay)
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
        newPuzzle = { name: name, fen: fen, move: move, box: firstBox, lastDrilledAt: 0 }
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
    { act: Review, vw: _ } ->
      case head state.reviewStack >>= getPuzzleByName state.puzzles of
        Just puzzleForReview ->
          state { view = ReviewingPuzzle puzzleForReview.name puzzleForReview.fen Nothing true }
        Nothing ->
          case state.view of
            MainMenu _ _ ->
              state { alert = Just NoPuzzlesForReview }
            ReviewingPuzzle _ _ _ _ ->
              state { alert = Just AllPuzzlesReviewed }
            _ ->
              state
    {
      act: AttemptPuzzle move now varianceFactor, 
      vw: ReviewingPuzzle puzzleName fen Nothing true
    } ->
      let
        updateStateView s = s { view = ReviewingPuzzle puzzleName fen (Just move) false }
        updateStateReviewStack s = s { reviewStack = s.reviewStack # tail # fromMaybe [] }
        indexInPuzzles = findIndex (\p -> p.name == puzzleName) state.puzzles
        isCorrect = indexInPuzzles
          >>= (\i -> state.puzzles !! i)
          # map (\p -> p.move == move)
        updateStatePuzzles s = s { puzzles =
          (lift2 Tuple isCorrect indexInPuzzles)
            >>= (\tup -> updatePuzzles (fst tup) (snd tup) now varianceFactor s.puzzles)
            # fromMaybe s.puzzles }
      in
        state
          # updateStateView
          # updateStateReviewStack
          # updateStatePuzzles
    {
      act: AttemptPuzzle move _ _, 
      vw: ReviewingPuzzle puzzleName fen Nothing false
    } ->
      state { view = ReviewingPuzzle puzzleName fen (Just move) false }
    { act: Retry, vw: ReviewingPuzzle puzzleName fen (Just move) false } ->
      state { view = ReviewingPuzzle puzzleName fen Nothing false }
    { act: _, vw: _ } ->
      state

-- Auto-incrementing the name removes the hassle of duplicate names
-- "endgame" -> "endgame" if no puzzles already exist
-- "endgame" -> "endgame #2" if one puzzle already exists with the names "endgame"
-- "endgame" -> "endgame #3" if two puzzles already exist with the names "endgame" and "endgame #2"
-- Behavior is undefined for untrimmed names
incrementName :: Array Puzzle -> String -> String
incrementName puzzles name = 
  if isNothing (findIndex (\p -> p.name == name) puzzles) then
    name
  else
    let
      previousIncrementsRegex = unsafePartial (fromRight (regex "^(\\S.*\\S)\\s+#([1-9][0-9]*)$" multiline))
      currentIncrement = puzzles
        # (map \p -> p.name) 
        # (mapMaybe (match previousIncrementsRegex))
        # (mapMaybe \nonEmptyArr -> 
            case toArray nonEmptyArr of
              [Just _, Just _, Just intString] -> fromString intString
              _ -> Nothing)
        # (foldr max 1)
        # add 1
    in
      name <> " #" <> show currentIncrement

-- Decides which puzzles are "up for review" by looking at the last-drilled-at date and what "box" they are in.
-- "box" refers to the flashcards-in-boxes spaced repetition system which this app gets inspiration from.
-- https://en.wikipedia.org/wiki/Leitner_system 
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

-- Defined with this param order for convenient use with bind
getPuzzleByName :: Array Puzzle -> Name -> Maybe Puzzle
getPuzzleByName puzzles name = 
  find (\puzzle -> puzzle.name == name) puzzles

updatePuzzles :: Boolean -> Int -> TimestampSeconds -> VarianceFactor -> Array Puzzle -> Maybe (Array Puzzle)
updatePuzzles isCorrect index now varianceFactor puzzles =
  let
    oldBox = (puzzles !! index) # map (\p -> p.box) # fromMaybe 1
    newBox = if isCorrect then min maxBox (oldBox * factorForNextBox) else firstBox
    newTimestamp = now - (newBox # mul secondsInADay # toNumber # mul varianceFactor # round)
  in
    alterAt index (\p -> Just p { box = newBox, lastDrilledAt = newTimestamp }) puzzles