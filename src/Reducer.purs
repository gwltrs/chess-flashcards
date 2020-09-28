module Reducer where

import Types (Action(..), FEN, PuzzleName, State, View(..))
import Data.Maybe (Maybe(..))

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
    CloseAlert -> 
      state { alert = Nothing }

getPuzzleName :: State -> PuzzleName
getPuzzleName state =
  case state.view of
    MainMenu name _ -> name
    _ -> ""

getFEN :: State -> FEN
getFEN state = 
  case state.view of
    MainMenu _ fen -> fen
    _ -> ""