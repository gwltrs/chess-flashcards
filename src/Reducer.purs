module Reducer where

import Data.Maybe (Maybe(..), isJust)
import Data.HeytingAlgebra ((||), not)
import Data.String (trim, length)
import Data.Eq ((==))
import Data.Array (elemIndex)
import Data.Function ((#))
import Data.Functor (map)

import Types (Action(..), FEN, PuzzleName, State, View(..), Alert(..))
import Chess (fenIsValid)

reducer :: State -> Action -> State
reducer state action =
  case { act: action, vw: state.view } of
    { act: NewFile, vw: _ } ->
      state { view = MainMenu "" "" }
    { act: LoadFile, vw: _ } ->
      state { view = MainMenu "" "" }
    { act: UpdatePuzzleName newName, vw: MainMenu _ currentFEN } ->
      state { view = MainMenu newName currentFEN }
    { act: UpdateFEN newFEN, vw: MainMenu currentName _ } ->
      state { view = MainMenu currentName newFEN }
    { act: CreatePuzzle, vw: MainMenu name fen } ->
      let
        trimmedName = trim name
        trimmedFEN = trim fen
      in
        if (length trimmedName) == 0 || (length trimmedFEN) == 0 then
          state { alert = Just MissingNameOrFEN }
        else if not (fenIsValid fen) then 
          state { alert = Just InvalidFEN }
        else if state.puzzles # (map \p -> p.name) # elemIndex trimmedName # isJust then
          state { alert = Just DuplicateName }
        else if state.puzzles # (map \p -> p.fen) # elemIndex trimmedFEN # isJust then
          state { alert = Just DuplicateFEN }
        else
          state
      
    { act: BackToMain, vw: _ } ->
      state { view = MainMenu "" "" }
    { act: CloseAlert, vw: _ } -> 
      state { alert = Nothing }
    { act: _, vw: _ } ->
      state