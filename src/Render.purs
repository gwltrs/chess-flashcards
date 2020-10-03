module Render where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HC
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Function ((#))
import Data.Functor (map)

import Types (Action(..), State, View(..), Alert(..))
  
render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div_ [contentDiv, alertDiv]
  where 

    contentDiv :: H.ComponentHTML Action () m
    contentDiv = 
      case state.view of
        LoadingFile -> 
          HH.div_
            [ 
              menuButton "New" NewFile,
              menuButton "Load" LoadFile
            ]
        MainMenu _ _ -> 
          HH.div_
            [ 
              menuButton "Save" SaveFile,
              HH.br_,
              menuButton "Review" Review,
              HH.br_,
              HH.input [ HP.class_ (HC.ClassName "textField"), HE.onValueChange \val -> Just (UpdatePuzzleName val) ],
              HH.input [ HP.class_ (HC.ClassName "textField"), HE.onValueChange \val -> Just (UpdateFEN val) ],
              menuButton "Create" CreatePuzzle
            ]
        CreatingPuzzle _ _ _ ->
          HH.div_
            [ 
              menuButton "Back" BackToMain,
              menuButton "Save" SavePuzzle
            ]
            
    alertDiv :: H.ComponentHTML Action () m
    alertDiv = 
      if isJust state.alert
      then 
        HH.div
          [
            HP.class_ (HC.ClassName "modalBackground")
          ]
          [ 
            HH.div
              [
                HP.class_ (HC.ClassName "modalContent")
              ]
              [ 
                HH.button 
                  [ 
                    HE.onClick \_ -> Just CloseAlert,
                    HP.class_ (HC.ClassName "closeButton")
                  ]
                  [ HH.text "Close" ],
                HH.span
                  [ HP.class_ (HC.ClassName "alertText") ]
                  [ state.alert # map alertText # fromMaybe "" # HH.text ]
              ] 
          ] 
      else 
        HH.div_ []


menuButton :: forall w i. String -> w -> HC.HTML i w
menuButton text action = 
  HH.button 
    [ 
      HP.class_ (HC.ClassName "menuButton"), 
      HE.onClick \_ -> Just action
    ] 
    [ 
      HH.text text
    ]

alertText :: Alert -> String
alertText = case _ of
  MissingNameOrFEN -> "Missing name or FEN"
  InvalidFEN -> "Invalid FEN"
  DuplicateName -> "Duplicate name"
  DuplicateFEN -> "Duplicate FEN"