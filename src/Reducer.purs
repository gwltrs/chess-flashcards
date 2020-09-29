module Reducer where

import Types (Action(..), FEN, PuzzleName, State, View(..))
import Data.Maybe (Maybe(..))

reducer :: State -> Action -> State
reducer state action =
  case { act: action, vw: state.view } of
    { act: NewFile, vw: _ } ->
      state { view = MainMenu "" "" }
    { act: LoadFile, vw: _ } ->
      state { view = MainMenu "" "" }
    { act: UpdatePuzzleName newName, vw: MainMenu _ currentFEN } ->
      state { view = MainMenu newName currentFEN }
    { act: UpdateFEN newFEN, vw: MainMenu currentName _ }  ->
      state { view = MainMenu currentName newFEN }
    { act: CreatePuzzle, vw: _ } ->
      state
    { act: BackToMain, vw: _ } ->
      state { view = MainMenu "" "" }
    { act: CloseAlert, vw: _ } -> 
      state { alert = Nothing }
    { act: _, vw: _ } ->
      state