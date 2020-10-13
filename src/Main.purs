module Main where

import Prelude (Unit, bind, unit, discard)

import Data.Maybe (Maybe(..))
import Effect (Effect)
--import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

import Types
import Reducer (reducer)
import Render (render)
import Chess (getMove)

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
handleAction action = case action of
  CreatePuzzle -> do
    H.modify_ \state -> reducer state action
    state <- H.get
    move <- H.liftAff (getMove (boardFEN state) "c4d5")
    H.modify_ \state2 -> reducer state2 BackToMain
  _ ->
    H.modify_ \state -> reducer state action
  where
    boardFEN state = case state.view of
      CreatingPuzzle _ fen _ -> fen
      _ -> ""