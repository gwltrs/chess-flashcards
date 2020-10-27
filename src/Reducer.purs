module Reducer where

import Prelude ((-), (>), compare, (>>=), (*))
import Control.Apply (lift2)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.HeytingAlgebra ((||), not)
import Data.String (trim, length)
import Data.Eq ((==))
import Data.Array (elemIndex, filter, mapMaybe, sortBy, head, tail, findIndex, (!!), alterAt)
import Data.Array.NonEmpty (toArray)
import Data.Function ((#))
import Data.Functor (map)
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (RegexFlags(..))
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

import Types (Action(..), Alert(..), Puzzle, State, View(..), Name, TimestampSeconds)
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
        updateView s = s { view = ReviewingPuzzle puzzleName fen (Just move) false }
        updateReviewStack s = s { reviewStack = s.reviewStack # tail # fromMaybe [] }
        indexInPuzzles = findIndex (\p -> p.name == puzzleName) state.puzzles
        isCorrect = indexInPuzzles
          >>= (\i -> state.puzzles !! i)
          # map (\p -> p.move == move)
        updatePuzzles s = (lift2 Tuple isCorrect indexInPuzzles)
          >>= (\tup -> alterAt
                      (snd tup)
                      (\p -> 
                        let
                          newBox = if fst tup then min maxBox (p.box * factorForNextBox) else firstBox
                        in
                          Just p {
                            box = newBox,
                            lastDrilledAt = now - (newBox # mul secondsInADay # toNumber # mul varianceFactor # round) })
                      s.puzzles)
          # map (\newPuzzles -> s { puzzles = newPuzzles })
          # fromMaybe s
      in
        state
          # updateView
          # updateReviewStack
          # updatePuzzles
    {
      act: AttemptPuzzle move _ _, 
      vw: ReviewingPuzzle puzzleName fen Nothing false
    } ->
      state { view = ReviewingPuzzle puzzleName fen (Just move) false }
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

-- Defined with this param order for convenient use with bind
getPuzzleByName :: Array Puzzle -> Name -> Maybe Puzzle
getPuzzleByName puzzles name = 
  find (\puzzle -> puzzle.name == name) puzzles