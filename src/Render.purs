module Render where

import Prelude ((>>>), (>>=), (==))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HC
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Semigroup ((<>))
import Data.Functor ((<#>))
import Data.Function ((#))
import Data.Array (singleton, findIndex, (!!))

import Types (Action(..), State, View(..), Alert(..))
import File (openFileDialogInput)
import Constants (nextButtonID)

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div_ [menuDiv, chessboardDiv, openFileDialogInput]
  where 
    menuDiv :: H.ComponentHTML Action () m
    menuDiv = 
      case state.view of
        LoadingFile -> 
          HH.div_
            [ 
              menuButton Nothing "New" NewFile true,
              menuButton Nothing "Load" OpenFileDialog true
            ]
        MainMenu _ _ -> 
          HH.div_
            [ 
              menuButton Nothing "Save" SaveFile true,
              HH.br_,
              menuButton Nothing "Review" Review true,
              HH.br_,
              menuInput "Name" UpdatePuzzleName,
              menuInput "FEN" UpdateFEN,
              menuButton Nothing "Create" CreatePuzzle true
            ]
        CreatingPuzzle puzzleName _ move ->
          HH.div_
            [ 
              menuButton Nothing "Back" BackToMain true,
              HH.input [
                HP.class_ (HC.ClassName "label"),
                HP.value puzzleName,
                HP.readOnly true
              ],
              menuButton Nothing "Save" StartSavingPuzzle (isJust move)
            ]
        ReviewingPuzzle _ _ _ firstAttempt ->
          HH.div_
            [
              menuButton Nothing "Back" BackToMain true,
              menuButton Nothing "Retry" Retry (isJust firstAttempt),
              menuButton (Just nextButtonID) "Next" Review (nextButtonIsEnabled state),
              menuButton Nothing "Show Name" ShowName (isJust firstAttempt),
              menuButton Nothing "Copy FEN" CopyFEN (isJust firstAttempt)
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

menuButton :: forall w i. Maybe String -> String -> w -> Boolean -> HC.HTML i w
menuButton id text action isEnabled   = 
  HH.button 
    (
      [ 
        HP.class_ (HC.ClassName "menuButton"), 
        HE.onClick \_ -> Just action,
        HP.enabled isEnabled
      ] <> 
      (id <#> HP.id_ <#> singleton # fromMaybe [])
    )
    [ 
      HH.text text
    ]

menuInput :: forall w i. String -> (String -> w) -> HC.HTML i w
menuInput placeholder actionMaker =
  HH.input
    [
      HP.placeholder placeholder,
      HP.class_ (HC.ClassName "textField"),
      HE.onValueChange (actionMaker >>> Just)
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

nextButtonIsEnabled :: State -> Boolean
nextButtonIsEnabled state = 
  case state.view of
    ReviewingPuzzle puzzleName _ (Just move) _ ->
      findIndex (\p -> p.name == puzzleName) state.puzzles
        >>= (\i -> state.puzzles !! i)
        <#> (\p -> p.move == move)
        # fromMaybe false
    _ ->
      false