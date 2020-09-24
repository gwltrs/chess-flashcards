module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State = 
  { 
    puzzles :: Array Puzzle, 
    reviewStack :: Array PuzzleName, -- Names of the puzzles that are up for review
    view :: View
  }

type Puzzle = 
  { 
    name :: PuzzleName,
    fen :: FEN,
    move :: Move,
    box :: Int,
    lastDrilledAt :: Int
  }

type PuzzleName = String
type FEN = String

type Move = 
  { 
    from :: Space, 
    to :: Space, 
    underpromotion :: Maybe Underpromotion 
  }

type Space = String -- In the format of 'a8', 'h4', 'f3', etc.
data Underpromotion = Bishop | Knight | Rook

data View = 
  LoadingFile | 
  MainMenu PuzzleName FEN | 
  CreatingPuzzle

data Action = 
  NewFile | 
  LoadFile | 
  SaveFile | 
  Review | 
  UpdatePuzzleName PuzzleName |
  UpdateFEN FEN |
  CreatePuzzle | 
  BackToMain |
  SavePuzzle

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { 
      initialState, 
      render, 
      eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { puzzles: [], reviewStack: [], view: LoadingFile }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  case state.view of
    LoadingFile -> 
      HH.div_
        [ 
          HH.button [ HE.onClick \_ -> Just NewFile ] [ HH.text "New" ],
          HH.button [ HE.onClick \_ -> Just LoadFile ] [ HH.text "Load" ]
        ]
    MainMenu _ _ -> 
      HH.div_
        [ 
          HH.button [ HE.onClick \_ -> Just SaveFile ] [ HH.text "Save" ],
          HH.br_,
          HH.button [ HE.onClick \_ -> Just Review ] [ HH.text "Review" ],
          HH.br_,
          HH.input [ HE.onValueChange \val -> Just (UpdatePuzzleName val) ],
          HH.input [ HE.onValueChange \val -> Just (UpdateFEN val) ],
          HH.button [ HE.onClick \_ -> Just CreatePuzzle ] [ HH.text "Create" ]
        ]
    CreatingPuzzle -> 
      HH.div_
        [ 
          HH.button [ HE.onClick \_ -> Just BackToMain ] [ HH.text "Back" ],
          HH.button [ HE.onClick \_ -> Just SavePuzzle ] [ HH.text "Save" ]
        ]

getFEN :: State -> FEN
getFEN state =
  "asdf"

getPuzzleName :: State -> PuzzleName
getPuzzleName state =
  "asdf"


handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  NewFile ->
    H.modify_ \state -> state { view = MainMenu "" "" }
  LoadFile ->
    H.modify_ \state -> state { view = MainMenu "" "" }
  SaveFile ->
    H.modify_ \state -> state
  Review ->
    H.modify_ \state -> state
  UpdatePuzzleName puzzleName ->
    H.modify_ \state -> state { view = MainMenu puzzleName (getFEN state) }
  UpdateFEN fen ->
    H.modify_ \state -> state { view = MainMenu (getPuzzleName state) fen }
  CreatePuzzle ->
    H.modify_ \state -> state
  BackToMain ->
    H.modify_ \state -> state { view = MainMenu "" "" }
  SavePuzzle ->
    H.modify_ \state -> state { view = MainMenu "" "" }