module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Types
import Reducer (reducer)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

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

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction action =
  H.modify_ \state -> reducer state action

asdf :: Int -> Int
asdf x = x + 1