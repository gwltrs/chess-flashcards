module Render where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HC
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))

import Types (Action(..), State, View(..), Alert(..))
  
render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div_ [menuDiv, chessboardDiv]
  where 
    menuDiv :: H.ComponentHTML Action () m
    menuDiv = 
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
        CreatingPuzzle puzzleName _ _ ->
          HH.div_
            [ 
              menuButton "Back" BackToMain,
              HH.input [
                HP.class_ (HC.ClassName "label"),
                HP.value puzzleName,
                HP.readOnly true
              ],
              menuButton "Save" SavePuzzle
            ]

    chessboardDiv :: H.ComponentHTML Action () m
    chessboardDiv = 
      let 
        noDisplayClassArray = if boardIsVisible state then [] else [ HP.class_ (HC.ClassName "noDisplay") ]
      in
        HH.div
          ([
            HP.id_ "chessboard"
          ] <> noDisplayClassArray)
          []

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
  MissingNameOrFEN -> "Missing name and/or FEN"
  InvalidFEN -> "Invalid FEN"
  DuplicateName -> "Duplicate name"
  DuplicateFEN -> "Duplicate FEN"

boardIsVisible :: State -> Boolean
boardIsVisible state = 
  case state.view of 
    CreatingPuzzle _ _ _ -> true
    _ -> false