module Reducer where

import Data.Maybe (Maybe(..))

import Types

reducer :: State -> Action -> State
reducer state action =
  case action of
    NewFile ->
      state { view = MainMenu "" "" }
    LoadFile ->
      state { view = MainMenu "" "" }
    SaveFile ->
      state
    Review ->
      state
    UpdatePuzzleName puzzleName ->
      state { view = MainMenu puzzleName (getFEN state) }
    UpdateFEN fen ->
      state { view = MainMenu (getPuzzleName state) fen }
    CreatePuzzle ->
      state
    BackToMain ->
      state { view = MainMenu "" "" }
    SavePuzzle ->
      state { view = MainMenu "" "" }

getFEN :: State -> FEN
getFEN state =
  "asdf"

getPuzzleName :: State -> PuzzleName
getPuzzleName state =
  "asdf"
