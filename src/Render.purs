module Render where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HC
import Data.Maybe (Maybe(..), isJust)

import Types (Action(..), State, View(..))
  
render :: forall m. State -> H.ComponentHTML Action () m
render state =

  let 

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
      then HH.div_ [ HH.button [ HE.onClick \_ -> Just CloseAlert ] [ HH.text "Close" ] ] 
      else HH.div_ []

  in 
  
    HH.div_ [contentDiv, alertDiv]

menuButton text action = 
  HH.button 
    [ 
      HP.class_ (HC.ClassName "menuButton"), 
      HE.onClick \_ -> Just action
    ] 
    [ 
      HH.text text
    ]