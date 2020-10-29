module Render where

import Prelude (not)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HC
import Data.Maybe (Maybe(..), isJust)
import Data.Semigroup ((<>))

import Types (Action(..), State, View(..), Alert(..))
import File (openFileDialogInput)

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div_ [menuDiv, chessboardDiv, openFileDialogInput]
  where 
    menuDiv :: H.ComponentHTML Action () m
    menuDiv = 
      case state.view of
        LoadingFile -> 
          HH.div_
            [ 
              menuButton "New" NewFile true,
              menuButton "Load" OpenFileDialog true
            ]
        MainMenu _ _ -> 
          HH.div_
            [ 
              menuButton "Save" SaveFile true,
              HH.br_,
              menuButton "Review" Review true,
              HH.br_,
              HH.input [ HP.class_ (HC.ClassName "textField"), HE.onValueChange \val -> Just (UpdatePuzzleName val) ],
              HH.input [ HP.class_ (HC.ClassName "textField"), HE.onValueChange \val -> Just (UpdateFEN val) ],
              menuButton "Create" CreatePuzzle true
            ]
        CreatingPuzzle puzzleName _ move ->
          HH.div_
            [ 
              menuButton "Back" BackToMain true,
              HH.input [
                HP.class_ (HC.ClassName "label"),
                HP.value puzzleName,
                HP.readOnly true
              ],
              menuButton "Save" SavePuzzle (isJust move)
            ]
        ReviewingPuzzle _ _ _ isFirst ->
          HH.div_
            [
              menuButton "Back" BackToMain true,
              menuButton "Retry" Retry (not isFirst),
              menuButton "Next" Review (not isFirst),
              menuButton "Show Name" ShowName (not isFirst),
              menuButton "Copy FEN" CopyFEN (not isFirst)
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

menuButton :: forall w i. String -> w -> Boolean -> HC.HTML i w
menuButton text action isEnabled = 
  HH.button 
    [ 
      HP.class_ (HC.ClassName "menuButton"), 
      HE.onClick \_ -> Just action,
      HP.enabled isEnabled
    ] 
    [ 
      HH.text text
    ]

alertText :: Alert -> String
alertText = case _ of
  MissingNameOrFEN -> "Empty field(s): provide a name and FEN for the new puzzle"
  InvalidFEN -> "Invalid FEN"
  DuplicateFEN -> "A puzzle with this FEN already exists"
  InvalidFile -> "The selected file doesn't have the expected format"
  NoPuzzlesForReview -> "No puzzles are up for review at this time"
  AllPuzzlesReviewed -> "You completed all the puzzles that are up for review"
  ThisIsTheName name -> name

boardIsVisible :: State -> Boolean
boardIsVisible state = 
  case state.view of 
    CreatingPuzzle _ _ _ -> true
    ReviewingPuzzle _ _ _ _ -> true
    _ -> false