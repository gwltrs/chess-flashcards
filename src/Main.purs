module Main where

import Prelude (Unit, bind, unit, discard, (/), (==), (*))

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Function ((#))
import Effect (Effect)
import Effect.Now (now)
import Effect.Aff.Class (class MonadAff)
import Data.DateTime.Instant (unInstant)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (alert)
import Control.Applicative (pure)
import Data.Functor (map)
import Data.Newtype (unwrap)
import Data.Int (round)
import Data.Foldable (find)
import Effect.Random (random)

import Types
import Reducer (reducer)
import Render (render, alertText)
import Chess (getMove)
import File (saveFile, openFileDialog)
import PuzzlesJSON (makePuzzlesJSON)
import Constants (maxVariance)
import Clipboard (copyToClipboard)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { 
      initialState, 
      render, 
      eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { puzzles: [], reviewStack: [], view: LoadingFile, alert: Nothing }

-- Implementing IO/async logic here but delegating pure logic to the reducer
handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction action = do

  -- First we apply the action and get the new state
  H.modify_ \state -> reducer state action
  stateAfterAction <- H.get

  -- For testability reasons we want the alert to be represented in the state
  -- If a "Just alert" is found, the alert is removed from the state and rendered
  case stateAfterAction.alert of
    Just alertInState -> do
      w <- H.liftEffect window
      H.liftEffect (alert (alertText alertInState) w)
      H.modify_ \stateWithAlert -> stateWithAlert { alert = Nothing }
    Nothing ->
      pure unit

  -- Firing off the rest of the Effects and Affs
  case action of
    CreatePuzzle -> do
      move <- H.liftAff (getMove (boardFEN stateAfterAction) "")
      nowTimestamp <- H.liftEffect nowInSeconds
      handleAction (AddMoveToNewPuzzle move)
    Review -> do
      move <- H.liftAff (getMove (boardFEN stateAfterAction) (expectedMove stateAfterAction))
      random0To1 <- H.liftEffect random
      nowTimestamp <- H.liftEffect nowInSeconds
      handleAction (AttemptPuzzle move nowTimestamp (maxVariance * random0To1))
    Retry -> do
      move <- H.liftAff (getMove (boardFEN stateAfterAction) (expectedMove stateAfterAction))
      random0To1 <- H.liftEffect random
      nowTimestamp <- H.liftEffect nowInSeconds
      handleAction (AttemptPuzzle move nowTimestamp (maxVariance * random0To1))
    SaveFile -> do
      H.liftEffect (saveFile "chess-flashcards-data.txt" (makePuzzlesJSON stateAfterAction.puzzles))
      pure unit
    OpenFileDialog -> do
      textInFile <- H.liftAff openFileDialog
      nowTimestamp <- H.liftEffect nowInSeconds
      handleAction (LoadFile textInFile nowTimestamp)
    CopyFEN -> do
      H.liftEffect (copyToClipboard (boardFEN stateAfterAction))
    _ ->
      pure unit

-- TODO: Figure out how to lift Maybe into HalogenM so this function can return Maybe instead of empty string
boardFEN :: State -> FEN
boardFEN state = case state.view of
  CreatingPuzzle _ fen _ -> fen
  ReviewingPuzzle _ fen _ _ -> fen
  _ -> ""

-- TODO: Figure out how to lift Maybe into HalogenM so this function can return Maybe instead of empty string
expectedMove :: State -> Move
expectedMove state = case state.view of
  ReviewingPuzzle puzzleName _ _ _ ->
    state.puzzles # find (\p -> p.name == puzzleName) # map (\p -> p.move) # fromMaybe ""
  _ -> 
    ""

nowInSeconds :: Effect Int
nowInSeconds = now
  # map unInstant 
  # map unwrap 
  # map (\x -> x / 1000.0)
  # map round