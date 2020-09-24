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

type State = { puzzles :: Array Puzzle, reviewStack :: Array PuzzleName, view :: View }
type Puzzle = 
  { 
    name :: PuzzleName,
    fen :: String,
    move :: Move,
    box :: Int,
    lastDrilledAt :: Int
  }
type PuzzleName = String
type Move = { from :: Space, to :: Space, underpromotion :: Maybe Underpromotion }
type Space = String -- In the format of 'a8', 'h4', 'f3', etc.
data Underpromotion = Bishop | Knight | Rook
data View = LoadingFile | MainMenu

data Action = NewFile | LoadFile

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
    MainMenu -> 
      HH.div_
        [ 
          HH.button [ HE.onClick \_ -> Just NewFile ] [ HH.text "Save" ],
          HH.button [ HE.onClick \_ -> Just LoadFile ] [ HH.text "Review" ],
          HH.button [ HE.onClick \_ -> Just LoadFile ] [ HH.text "Create" ]
        ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  NewFile ->
    H.modify_ \state -> state { view = MainMenu }
  LoadFile ->
    H.modify_ \state -> state { view = MainMenu }