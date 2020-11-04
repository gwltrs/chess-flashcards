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
import Data.Functor ((<#>))
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
  case Tuple action state.view of
    Tuple NewFile _ ->
      state { view = MainMenu "" "" }
    Tuple (UpdatePuzzleName newName) (MainMenu _ currentFEN) ->
      state { view = MainMenu newName currentFEN }
    Tuple (UpdateFEN newFEN) (MainMenu currentName _) ->
      state { view = MainMenu currentName newFEN }
    Tuple CreatePuzzle (MainMenu name fen) ->
      let
        trimmedName = trim name
        trimmedFEN = trim fen
        sanitizedFEN = sanitizeFEN trimmedFEN
      in
        if (length trimmedName) == 0 || (length trimmedFEN) == 0 then
          state { alert = Just MissingNameOrFEN }
        else if not (fenIsValid fen) then 
          state { alert = Just InvalidFEN }
        else if state.puzzles <#> (\p -> p.fen) # elemIndex sanitizedFEN # isJust then
          state { alert = Just DuplicateFEN }
        else
          state { view = CreatingPuzzle (incrementName state.puzzles trimmedName) sanitizedFEN Nothing }
    Tuple BackToMain _ ->
      state { view = MainMenu "" "" }
    Tuple (AddMoveToNewPuzzle move) (CreatingPuzzle name fen _) ->
      state { view = CreatingPuzzle name fen (Just move) }
    Tuple SavePuzzle (CreatingPuzzle name fen (Just move)) ->
      let
        newPuzzle = { name: name, fen: fen, move: move, box: firstBox, lastDrilledAt: 0 }
        comparePuzzles l r =
          localeCompare l.name r.name
      in
        state { puzzles = sortBy comparePuzzles (state.puzzles <> [newPuzzle]), view = MainMenu "" "" }
    Tuple (LoadFile fileString currentTimestamp) LoadingFile ->
      case parsePuzzlesJSON fileString of
        Just puzzles ->
          state { 
            puzzles = puzzles, 
            reviewStack = (generateReviewStack currentTimestamp puzzles),
            view = MainMenu "" "" }
        Nothing -> 
          state { alert = Just InvalidFile }
    Tuple Review _ ->
      case head state.reviewStack >>= getPuzzleByName state.puzzles of
        Just puzzleForReview ->
          state { view = ReviewingPuzzle puzzleForReview.name puzzleForReview.fen Nothing Nothing }
        Nothing ->
          case state.view of
            MainMenu _ _ ->
              state { alert = Just NoPuzzlesForReview }
            ReviewingPuzzle _ _ _ _ ->
              state { alert = Just AllPuzzlesReviewed }
            _ ->
              state
    Tuple (AttemptPuzzle move now varianceFactor) (ReviewingPuzzle puzzleName fen Nothing Nothing) ->
      let
        updateStateReviewStack s = s { reviewStack = s.reviewStack # tail # fromMaybe [] }
        indexInPuzzles = findIndex (\p -> p.name == puzzleName) state.puzzles
        isCorrect = indexInPuzzles
          >>= (\i -> state.puzzles !! i)
          <#> (\p -> p.move == move)
        updateStateView s = s { view = 
          isCorrect
            <#> Just
            <#> (ReviewingPuzzle puzzleName fen (Just move))
            # (fromMaybe s.view)
        }
        updateStatePuzzles s = s { puzzles =
          (lift2 Tuple isCorrect indexInPuzzles)
            >>= (\tup -> updatePuzzles (fst tup) (snd tup) now varianceFactor s.puzzles)
            # fromMaybe s.puzzles }
      in
        state
          # updateStateView
          # updateStateReviewStack
          # updateStatePuzzles
    Tuple (AttemptPuzzle move _ _) (ReviewingPuzzle puzzleName fen Nothing (Just firstAttempt)) ->
      state { view = ReviewingPuzzle puzzleName fen (Just move) (Just firstAttempt) }
    Tuple Retry (ReviewingPuzzle puzzleName fen (Just move) (Just firstAttempt)) ->
      state { view = ReviewingPuzzle puzzleName fen Nothing (Just firstAttempt) }
    Tuple ShowName (ReviewingPuzzle puzzleName _ _ (Just firstAttempt)) -> 
      state { alert = Just (ThisIsTheName puzzleName) }
    Tuple _ _ ->
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
        <#> (\p -> p.name) 
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
      <#> (\p -> { name: p.name, overdue: nowSeconds - p.lastDrilledAt - (secondsInADay * p.box) })
      # (filter \tuple -> tuple.overdue > 0)
      # (sortBy \l r -> compare r.overdue l.overdue)
      <#> (\tuple2 -> tuple2.name)

-- Defined with this param order for convenient use with bind
getPuzzleByName :: Array Puzzle -> Name -> Maybe Puzzle
getPuzzleByName puzzles name = 
  find (\puzzle -> puzzle.name == name) puzzles

updatePuzzles :: Boolean -> Int -> TimestampSeconds -> VarianceFactor -> Array Puzzle -> Maybe (Array Puzzle)
updatePuzzles isCorrect index now varianceFactor puzzles =
  let
    oldBox = (puzzles !! index) <#> (\p -> p.box) # fromMaybe 1
    newBox = if isCorrect then min maxBox (oldBox * factorForNextBox) else firstBox
    newTimestamp = now - (newBox # mul secondsInADay # toNumber # mul varianceFactor # round)
  in
    alterAt index (\p -> Just p { box = newBox, lastDrilledAt = newTimestamp }) puzzles